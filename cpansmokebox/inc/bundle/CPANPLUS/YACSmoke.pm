package CPANPLUS::YACSmoke;

use strict;
use warnings;

use Carp;
use CPANPLUS::Backend;
use CPANPLUS::Configure;
use CPANPLUS::Error;
use CPANPLUS::Internals::Constants;
use POSIX qw( O_CREAT O_RDWR O_RDONLY );         # for SDBM_File
use SDBM_File;
use File::Fetch;
use IO::File;
use File::Spec::Functions;
use File::Path;
use Regexp::Assemble;
use Sort::Versions;
use Config::IniFiles;

use vars qw($VERSION);

use constant DATABASE_FILE => 'cpansmoke.dat';
use constant CONFIG_FILE   => 'cpansmoke.ini';
use constant RECENT_FILE   => 'RECENT';

require Exporter;

our @ISA = qw( Exporter );
our %EXPORT_TAGS = (
  'all'      => [ qw( mark test excluded purge flush ) ],
  'default'  => [ qw( mark test excluded ) ],
);

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT    = ( @{ $EXPORT_TAGS{'default'} } );

$VERSION = '0.45_01';

{
  my %Checked;
  my $TiedObj;


sub _connect_db {
  my $self = shift;
  return if $TiedObj;
  my $filename = catfile( $self->{conf}->get_conf('base'), DATABASE_FILE );
  $TiedObj = tie %Checked, 'SDBM_File', $filename, O_CREAT|O_RDWR, 0644;
  $self->{checked} = \%Checked;
}

sub _disconnect_db {
  my $self = shift;
  return unless $TiedObj;
  $TiedObj         = undef;
  $self->{checked} = undef;
  untie %Checked;
}

sub new {
  my $package = shift;
  my $nconf = shift if ref $_[0] and $_[0]->isa('CPANPLUS::Configure');

  $ENV{AUTOMATED_TESTING} = 1;
  $ENV{PERL_MM_USE_DEFAULT} = 1; # despite verbose setting
  $ENV{PERL_EXTUTILS_AUTOINSTALL} = '--defaultdeps';

  my $conf = $nconf || CPANPLUS::Configure->new();

  # Override configure settings
  $conf->set_conf( prereqs => 2 ); # force to ask callback
  $conf->set_conf( skiptest => 0 ); 
  $conf->set_conf( no_update => 1 )
    if glob( catfile( $conf->get_conf('base'), $conf->_get_source('stored') .'*'. STORABLE_EXT, ) );
  $conf->set_conf( dist_type => 'CPANPLUS::Dist::YACSmoke' ); # this is where the magic happens.
  $conf->set_conf( cpantest => 'dont_cc' ); # Yes, we want to report test results. But not CC
  $conf->set_conf( verbose => 1 ); # set verbosity to true.

  unless ( defined $ENV{MAILDOMAIN} ) {
     my $hostpart = ( split /\@/, ( $conf->get_conf( 'email' ) || 'smoker@cpantesters.org' ) )[1];
     $ENV{MAILDOMAIN} = $hostpart =~ /^(cpan\.org|gmail\.com)$/i ? 'cpantesters.org' : $hostpart;
  }

  if ( $^V gt v5.9.5 ) {
     $conf->set_conf( prefer_makefile => 0 ); # Prefer Build.PL if we have M::B
  }
  else {
     eval "require Module::Build"; 
     $conf->set_conf( prefer_makefile => 0 ) unless $@; # 
  }

  my $cb   = CPANPLUS::Backend->new($conf);

  my $exclude_dists;
  my $config_file = catfile( $conf->get_conf('base'), CONFIG_FILE );
  if ( -r $config_file ) {
     my $cfg = Config::IniFiles->new(-file => $config_file);
     my @list = $cfg->val( 'CONFIG', 'exclude_dists' );
     if ( @list ) {
        $exclude_dists = Regexp::Assemble->new();
        $exclude_dists->add( @list );
     }
  }

  my $self = bless { @_ }, $package;
  $self->{conf} = $conf;
  $self->{cpanplus} = $cb;
  $self->{exclude_dists} = $exclude_dists;
  $self->{allow_retries} = 'aborted|ungraded';
  return $self;
}

sub test {
  my $self;
  eval {
    if ( (ref $_[0]) && $_[0]->isa(__PACKAGE__) ) {
	$self = shift;
    }
  };
  $self ||= __PACKAGE__->new();
  $self->_connect_db();
  
  my @dists = @_;

  unless ( @dists ) {
     @dists = $self->_download_list();
  }

  my @mods;

  foreach my $dist ( @dists ) {
     my $mod = $self->{cpanplus}->parse_module( module => $dist );
     next unless $mod;
     my $package = $mod->package_name .'-'. $mod->package_version;
     my $grade = $self->{checked}->{$package} || 'ungraded';
     next if $self->_is_excluded_dist($package);
     next if $mod->is_bundle;
     next unless $grade =~ /$self->{allow_retries}/;
     push @mods, $mod;
  }

  $self->_disconnect_db();

  foreach my $mod ( @mods ) {
     eval {
		CPANPLUS::Error->flush();
		my $stat = $self->{cpanplus}->install( 
				modules  => [ $mod ],
				target   => 'create',
				allow_build_interactively => 0,
				# other settings now set via set_config() method
		);
     };
  }

  $self->{cpanplus}->save_state();
  return 1;
}

sub mark {
  my $self;
  eval {
    if ( (ref $_[0]) && $_[0]->isa(__PACKAGE__) ) {
	$self = shift;
    }
  };
  $self ||= __PACKAGE__->new();
  $self->_connect_db();

  my $distver = shift || '';
  my $grade   = lc shift || '';

  if ($grade) {
    my $mod = $self->{cpanplus}->parse_module( module => $distver );
    return error(qq{Invalid distribution "$distver"}) unless $mod;
    
    unless ($grade =~ /(pass|fail|unknown|na|none|ungraded|aborted|ignored)/) {
      return error("Invalid grade: '$grade'");
    }
    if ($grade eq "none") {
      $grade = undef;
    }

    $distver = $mod->package_name .'-'. $mod->package_version;
    $self->{checked}->{$distver} = $grade;
  }
  else {
    my @distros = ($distver ? ($distver) : $self->_download_list());
    foreach my $dist ( @distros ) {
       my $mod = $self->{cpanplus}->parse_module( module => $dist );
       next unless $mod;
       my $dist_ver = $mod->package_name .'-'. $mod->package_version;
       next if $self->_is_excluded_dist( $dist_ver );
       $grade = $self->{checked}->{$dist_ver};
       if ( $grade ) {
	 msg(qq{result for "$dist_ver" is "$grade"});
       }
       else {
	 msg(qq{no result for "$dist_ver"});
       }
    }
  }
  $self->_disconnect_db();
  return $grade if $distver;
}

sub excluded {
  my $self;
  eval {
    if ( (ref $_[0]) && $_[0]->isa(__PACKAGE__) ) {
	$self = shift;
    }
  };
  $self ||= __PACKAGE__->new();

  my @dists = @_;

  unless ( @dists ) {
     @dists = $self->_download_list();
  }

  my @mods;

  foreach my $dist ( @dists ) {
     my $mod = $self->{cpanplus}->parse_module( module => $dist );
     next unless $mod;
     my $package = $mod->package_name .'-'. $mod->package_version;
     next unless $self->_is_excluded_dist($package);
     msg(qq{EXCLUDED: "$package"});
     push @mods, $package;
  }

  return @mods;
}

sub purge {
  my $self;
  eval {
    if ( (ref $_[0]) && $_[0]->isa(__PACKAGE__) ) {
	$self = shift;
    }
  };
  $self ||= __PACKAGE__->new();
  my %config = ref($_[0]) eq 'HASH' ? %{ shift() } : ();
  $self->_connect_db();

  my $flush = $config{flush_flag} || 0;
  my %distvars;
  my $override = 0;

  if(@_) {
     $override = 1;
     for(@_) {
	next	unless(/^(.*)\-(.+)$/);
	push @{$distvars{$1}}, $2;
     }
  } 
  else {
     for(keys %{$self->{checked}}) {
	next	unless(/^(.*)\-(.+)$/);
	push @{$distvars{$1}}, $2;
     }
  }

  foreach my $dist (sort keys %distvars) {
     my $passed = $override;
     my @vers = sort { versioncmp($a, $b) } @{$distvars{$dist}};
     while(@vers) {
	my $vers = pop @vers;		# the latest
	if($passed) {
		msg("'$dist-$vers' ['".
					uc($self->{checked}->{"$dist-$vers"}).
					"'] has been purged");
		delete $self->{checked}->{"$dist-$vers"};
		if($flush) {
	          my $builddir =
                      catfile($self->_get_build_dir(), "$dist-$vers");
		      rmtree($builddir)	if(-d $builddir);
		}
	}
	elsif($self->{checked}->{"$dist-$vers"} eq 'pass') {
		$passed = 1;
	}
     }
  }

  $self->_disconnect_db();
  return 1;
}

sub flush {
  my $self;
  eval {
    if ( (ref $_[0]) && $_[0]->isa(__PACKAGE__) ) {
	$self = shift;
    }
  };
  $self ||= __PACKAGE__->new();
  my %config = ref($_[0]) eq 'HASH' ? %{ shift() } : ();
  $self->_connect_db();

  my $param = shift || 'all';
  my %dists;

  opendir(DIR, $self->_get_build_dir());
  while(my $dir = readdir(DIR)) {
	next if $dir =~ /^\.+$/;

	if($param eq 'old') {
		$dir =~ /(.*)-(.+)/;
		$dists{$1}->{$2} = "$dir";
	} 
	else {
		rmtree($dir);
		msg("'$dir' flushed");
	}
  }
  closedir(DIR);

  if($param eq 'old') {
	for my $dist (keys %dists) {
	  for(sort { versioncmp($a, $b) } keys %{$dists{$dist}}) {
	    rmtree($dists{$dist}->{$_});
		msg("'$dists{$dist}->{$_}' flushed");
	  }
	}
  }

  $self->_disconnect_db();
  return 1;
}

sub _is_excluded_dist {
  my $self = shift;
  my $dist = shift || return;
  return unless $self->{exclude_dists};
  return 1 if $dist =~ $self->{exclude_dists}->re();
}

sub _download_list {
  my $self  = shift;

  my $path  = catdir( CPANPLUS::Internals::Utils->_home_dir(), '.cpanplus' );
  my $local = catfile( $path, RECENT_FILE );

  my $hosts = $self->{conf}->get_conf('hosts');
  my $h_ind = 0;

  while ($h_ind < @$hosts) {
      my $remote = $hosts->[$h_ind]->{scheme} . '://'
                . catdir(
                        $hosts->[$h_ind]->{host},
                        $hosts->[$h_ind]->{path} . RECENT_FILE );

      my $ff = File::Fetch->new( uri => $remote );
      my $status = $ff->fetch( to => $path );
      last if $status;
      $h_ind++;
  }

  return ()   if(@$hosts == $h_ind); # no host accessible

  my @testlist;
  my $fh = IO::File->new($local)
    or croak("Cannot access local RECENT file [$local]: $!\n");
  while (<$fh>) {
    next    unless(/^authors/);
    next    unless(/\.(?:tar\.(?:bz2|gz|Z)|t(?:gz|bz)|(?<!ppm\.)zip|pm.gz)\n$/i);
    s!authors/id/!!;
    chomp;
    push @testlist, $_;
  }

  return @testlist;
}

sub _get_build_dir {
  my $self = shift;
  File::Spec->catdir(
        $self->{conf}->get_conf('base'),
        $self->{cpanplus}->_perl_version( perl => $^X ),
        $self->{conf}->_get_build('moddir')
  );
}

}

1;
__END__

