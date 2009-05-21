package Module::Load::Conditional;

use strict;

use Module::Load;
use Params::Check                       qw[check];
use Locale::Maketext::Simple Style  => 'gettext';

use Carp        ();
use File::Spec  ();
use FileHandle  ();
use version;

use constant ON_VMS  => $^O eq 'VMS';

BEGIN {
    use vars        qw[ $VERSION @ISA $VERBOSE $CACHE @EXPORT_OK 
                        $FIND_VERSION $ERROR $CHECK_INC_HASH];
    use Exporter;
    @ISA            = qw[Exporter];
    $VERSION        = '0.30';
    $VERBOSE        = 0;
    $FIND_VERSION   = 1;
    $CHECK_INC_HASH = 0;
    @EXPORT_OK      = qw[check_install can_load requires];
}

### this checks if a certain module is installed already ###
### if it returns true, the module in question is already installed
### or we found the file, but couldn't open it, OR there was no version
### to be found in the module
### it will return 0 if the version in the module is LOWER then the one
### we are looking for, or if we couldn't find the desired module to begin with
### if the installed version is higher or equal to the one we want, it will return
### a hashref with he module name and version in it.. so 'true' as well.
sub check_install {
    my %hash = @_;

    my $tmpl = {
            version => { default    => '0.0'    },
            module  => { required   => 1        },
            verbose => { default    => $VERBOSE },
    };

    my $args;
    unless( $args = check( $tmpl, \%hash, $VERBOSE ) ) {
        warn loc( q[A problem occurred checking arguments] ) if $VERBOSE;
        return;
    }

    my $file     = File::Spec->catfile( split /::/, $args->{module} ) . '.pm';
    my $file_inc = File::Spec::Unix->catfile( 
                        split /::/, $args->{module} 
                    ) . '.pm';

    ### where we store the return value ###
    my $href = {
            file        => undef,
            version     => undef,
            uptodate    => undef,
    };
    
    my $filename;

    ### check the inc hash if we're allowed to
    if( $CHECK_INC_HASH ) {
        $filename = $href->{'file'} = 
            $INC{ $file_inc } if defined $INC{ $file_inc };

        ### find the version by inspecting the package
        if( defined $filename && $FIND_VERSION ) {
            no strict 'refs';
            $href->{version} = ${ "$args->{module}"."::VERSION" }; 
        }
    }     

    ### we didnt find the filename yet by looking in %INC,
    ### so scan the dirs
    unless( $filename ) {

        DIR: for my $dir ( @INC ) {
    
            my $fh;
    
            if ( ref $dir ) {
                ### @INC hook -- we invoke it and get the filehandle back
                ### this is actually documented behaviour as of 5.8 ;)
    
                if (UNIVERSAL::isa($dir, 'CODE')) {
                    ($fh) = $dir->($dir, $file);
    
                } elsif (UNIVERSAL::isa($dir, 'ARRAY')) {
                    ($fh) = $dir->[0]->($dir, $file, @{$dir}{1..$#{$dir}})
    
                } elsif (UNIVERSAL::can($dir, 'INC')) {
                    ($fh) = $dir->INC->($dir, $file);
                }
    
                if (!UNIVERSAL::isa($fh, 'GLOB')) {
                    warn loc(q[Cannot open file '%1': %2], $file, $!)
                            if $args->{verbose};
                    next;
                }
    
                $filename = $INC{$file_inc} || $file;
    
            } else {
                $filename = File::Spec->catfile($dir, $file);
                next unless -e $filename;
    
                $fh = new FileHandle;
                if (!$fh->open($filename)) {
                    warn loc(q[Cannot open file '%1': %2], $file, $!)
                            if $args->{verbose};
                    next;
                }
            }
    
            ### store the directory we found the file in
            $href->{dir} = $dir;
    
            ### files need to be in unix format under vms,
            ### or they might be loaded twice
            $href->{file} = ON_VMS
                ? VMS::Filespec::unixify( $filename )
                : $filename;
    
            ### user wants us to find the version from files
            if( $FIND_VERSION ) {
                
                my $in_pod = 0;
                while ( my $line = <$fh> ) {
    
                    ### stolen from EU::MM_Unix->parse_version to address
                    ### #24062: "Problem with CPANPLUS 0.076 misidentifying
                    ### versions after installing Text::NSP 1.03" where a 
                    ### VERSION mentioned in the POD was found before
                    ### the real $VERSION declaration.
                    $in_pod = $line =~ /^=(?!cut)/  ? 1 : 
                              $line =~ /^=cut/      ? 0 : 
                              $in_pod;
                    next if $in_pod;
                    
                    ### try to find a version declaration in this string.
                    my $ver = __PACKAGE__->_parse_version( $line );

                    if( defined $ver ) {
                        $href->{version} = $ver;
        
                        last DIR;
                    }
                }
            }
        }
    }
    
    ### if we couldn't find the file, return undef ###
    return unless defined $href->{file};

    ### only complain if we're expected to find a version higher than 0.0 anyway
    if( $FIND_VERSION and not defined $href->{version} ) {
        {   ### don't warn about the 'not numeric' stuff ###
            local $^W;

            ### if we got here, we didn't find the version
            warn loc(q[Could not check version on '%1'], $args->{module} )
                    if $args->{verbose} and $args->{version} > 0;
        }
        $href->{uptodate} = 1;

    } else {
        ### don't warn about the 'not numeric' stuff ###
        local $^W;
        
        ### use qv(), as it will deal with developer release number
        ### ie ones containing _ as well. This addresses bug report
        ### #29348: Version compare logic doesn't handle alphas?
        ###
        ### Update from JPeacock: apparently qv() and version->new
        ### are different things, and we *must* use version->new
        ### here, or things like #30056 might start happening
        $href->{uptodate} = 
            version->new( $args->{version} ) <= version->new( $href->{version} )
                ? 1 
                : 0;
    }

    return $href;
}

sub _parse_version {
    my $self    = shift;
    my $str     = shift or return;
    my $verbose = shift or 0;

    ### skip commented out lines, they won't eval to anything.
    return if $str =~ /^\s*#/;
        
    ### the following regexp & eval statement comes from the 
    ### ExtUtils::MakeMaker source (EU::MM_Unix->parse_version) 
    ### Following #18892, which tells us the original
    ### regex breaks under -T, we must modifiy it so
    ### it captures the entire expression, and eval /that/
    ### rather than $_, which is insecure.
    my $taint_safe_str = do { $str =~ /(^.*$)/sm; $1 };
        
    if( $str =~ /(?<!\\)([\$*])(([\w\:\']*)\bVERSION)\b.*\=/ ) {
        
        print "Evaluating: $str\n" if $verbose;
        
        ### this creates a string to be eval'd, like:
        # package Module::Load::Conditional::_version;
        # no strict;
        # 
        # local $VERSION;
        # $VERSION=undef; do {
        #     use version; $VERSION = qv('0.0.3');
        # }; $VERSION        
        
        my $eval = qq{
            package Module::Load::Conditional::_version;
            no strict;

            local $1$2;
            \$$2=undef; do {
                $taint_safe_str
            }; \$$2
        };
        
        print "Evaltext: $eval\n" if $verbose;
        
        my $result = do {
            local $^W = 0;
            eval($eval); 
        };
        
        
        my $rv = defined $result ? $result : '0.0';

        print( $@ ? "Error: $@\n" : "Result: $rv\n" ) if $verbose;

        return $rv;
    }
    
    ### unable to find a version in this string
    return;
}

sub can_load {
    my %hash = @_;

    my $tmpl = {
        modules     => { default => {}, strict_type => 1 },
        verbose     => { default => $VERBOSE },
        nocache     => { default => 0 },
    };

    my $args;

    unless( $args = check( $tmpl, \%hash, $VERBOSE ) ) {
        $ERROR = loc(q[Problem validating arguments!]);
        warn $ERROR if $VERBOSE;
        return;
    }

    ### layout of $CACHE:
    ### $CACHE = {
    ###     $ module => {
    ###             usable  => BOOL,
    ###             version => \d,
    ###             file    => /path/to/file,
    ###     },
    ### };

    $CACHE ||= {}; # in case it was undef'd

    my $error;
    BLOCK: {
        my $href = $args->{modules};

        my @load;
        for my $mod ( keys %$href ) {

            next if $CACHE->{$mod}->{usable} && !$args->{nocache};

            ### else, check if the hash key is defined already,
            ### meaning $mod => 0,
            ### indicating UNSUCCESSFUL prior attempt of usage

            ### use qv(), as it will deal with developer release number
            ### ie ones containing _ as well. This addresses bug report
            ### #29348: Version compare logic doesn't handle alphas?
            ###
            ### Update from JPeacock: apparently qv() and version->new
            ### are different things, and we *must* use version->new
            ### here, or things like #30056 might start happening            
            if (    !$args->{nocache}
                    && defined $CACHE->{$mod}->{usable}
                    && (version->new( $CACHE->{$mod}->{version}||0 ) 
                        >= version->new( $href->{$mod} ) )
            ) {
                $error = loc( q[Already tried to use '%1', which was unsuccessful], $mod);
                last BLOCK;
            }

            my $mod_data = check_install(
                                    module  => $mod,
                                    version => $href->{$mod}
                                );

            if( !$mod_data or !defined $mod_data->{file} ) {
                $error = loc(q[Could not find or check module '%1'], $mod);
                $CACHE->{$mod}->{usable} = 0;
                last BLOCK;
            }

            map {
                $CACHE->{$mod}->{$_} = $mod_data->{$_}
            } qw[version file uptodate];

            push @load, $mod;
        }

        for my $mod ( @load ) {

            if ( $CACHE->{$mod}->{uptodate} ) {

                eval { load $mod };

                ### in case anything goes wrong, log the error, the fact
                ### we tried to use this module and return 0;
                if( $@ ) {
                    $error = $@;
                    $CACHE->{$mod}->{usable} = 0;
                    last BLOCK;
                } else {
                    $CACHE->{$mod}->{usable} = 1;
                }

            ### module not found in @INC, store the result in
            ### $CACHE and return 0
            } else {

                $error = loc(q[Module '%1' is not uptodate!], $mod);
                $CACHE->{$mod}->{usable} = 0;
                last BLOCK;
            }
        }

    } # BLOCK

    if( defined $error ) {
        $ERROR = $error;
        Carp::carp( loc(q|%1 [THIS MAY BE A PROBLEM!]|,$error) ) if $args->{verbose};
        return;
    } else {
        return 1;
    }
}

sub requires {
    my $who = shift;

    unless( check_install( module => $who ) ) {
        warn loc(q[You do not have module '%1' installed], $who) if $VERBOSE;
        return undef;
    }

    my $lib = join " ", map { qq["-I$_"] } @INC;
    my $cmd = qq[$^X $lib -M$who -e"print(join(qq[\\n],keys(%INC)))"];

    return  sort
                grep { !/^$who$/  }
                map  { chomp; s|/|::|g; $_ }
                grep { s|\.pm$||i; }
            `$cmd`;
}

1;

__END__

