package Archive::Extract;

use strict;

use Cwd                         qw[cwd];
use Carp                        qw[carp];
use IPC::Cmd                    qw[run can_run];
use FileHandle;
use File::Path                  qw[mkpath];
use File::Spec;
use File::Basename              qw[dirname basename];
use Params::Check               qw[check];
use Module::Load::Conditional   qw[can_load check_install];
use Locale::Maketext::Simple    Style => 'gettext';

### solaris has silly /bin/tar output ###
use constant ON_SOLARIS     => $^O eq 'solaris' ? 1 : 0;
use constant FILE_EXISTS    => sub { -e $_[0] ? 1 : 0 };

### VMS may require quoting upper case command options
use constant ON_VMS         => $^O eq 'VMS' ? 1 : 0;

### Windows needs special treatment of Tar options
use constant ON_WIN32       => $^O eq 'MSWin32' ? 1 : 0;

### we can't use this extraction method, because of missing
### modules/binaries:
use constant METHOD_NA      => []; 

### If these are changed, update @TYPES and the new() POD
use constant TGZ            => 'tgz';
use constant TAR            => 'tar';
use constant GZ             => 'gz';
use constant ZIP            => 'zip';
use constant BZ2            => 'bz2';
use constant TBZ            => 'tbz';
use constant Z              => 'Z';
use constant LZMA           => 'lzma';

use vars qw[$VERSION $PREFER_BIN $PROGRAMS $WARN $DEBUG 
            $_ALLOW_BIN $_ALLOW_PURE_PERL
         ];

$VERSION            = '0.32';
$PREFER_BIN         = 0;
$WARN               = 1;
$DEBUG              = 0;
$_ALLOW_PURE_PERL   = 1;    # allow pure perl extractors
$_ALLOW_BIN         = 1;    # allow binary extractors

# same as all constants
my @Types           = ( TGZ, TAR, GZ, ZIP, BZ2, TBZ, Z, LZMA ); 

local $Params::Check::VERBOSE = $Params::Check::VERBOSE = 1;


### see what /bin/programs are available ###
$PROGRAMS = {};
for my $pgm (qw[tar unzip gzip bunzip2 uncompress unlzma]) {
    $PROGRAMS->{$pgm} = can_run($pgm);
}

### mapping from types to extractor methods ###
my $Mapping = {  # binary program           # pure perl module
    is_tgz  => { bin => '_untar_bin',       pp => '_untar_at'   },
    is_tar  => { bin => '_untar_bin',       pp => '_untar_at'   },
    is_gz   => { bin => '_gunzip_bin',      pp => '_gunzip_cz'  },
    is_zip  => { bin => '_unzip_bin',       pp => '_unzip_az'   },
    is_tbz  => { bin => '_untar_bin',       pp => '_untar_at'   },
    is_bz2  => { bin => '_bunzip2_bin',     pp => '_bunzip2_bz2'},
    is_Z    => { bin => '_uncompress_bin',  pp => '_gunzip_cz'  },
    is_lzma => { bin => '_unlzma_bin',      pp => '_unlzma_cz'  },
};

{   ### use subs so we re-generate array refs etc for the no-overide flags
    ### if we don't, then we reuse the same arrayref, meaning objects store
    ### previous errors
    my $tmpl = {
        archive         => sub { { required => 1, allow => FILE_EXISTS }    },
        type            => sub { { default => '', allow => [ @Types ] }     },
        _error_msg      => sub { { no_override => 1, default => [] }        },
        _error_msg_long => sub { { no_override => 1, default => [] }        },
    };

    ### build accesssors ###
    for my $method( keys %$tmpl, 
                    qw[_extractor _gunzip_to files extract_path],
    ) {
        no strict 'refs';
        *$method = sub {
                        my $self = shift;
                        $self->{$method} = $_[0] if @_;
                        return $self->{$method};
                    }
    }

    ### constructor ###
    sub new {
        my $class   = shift;
        my %hash    = @_;
        
        ### see above why we use subs here and generate the template;
        ### it's basically to not re-use arrayrefs
        my %utmpl   = map { $_ => $tmpl->{$_}->() } keys %$tmpl;

        my $parsed = check( \%utmpl, \%hash ) or return;

        ### make sure we have an absolute path ###
        my $ar = $parsed->{archive} = File::Spec->rel2abs( $parsed->{archive} );

        ### figure out the type, if it wasn't already specified ###
        unless ( $parsed->{type} ) {
            $parsed->{type} =
                $ar =~ /.+?\.(?:tar\.gz|tgz)$/i     ? TGZ   :
                $ar =~ /.+?\.gz$/i                  ? GZ    :
                $ar =~ /.+?\.tar$/i                 ? TAR   :
                $ar =~ /.+?\.(zip|jar|par)$/i       ? ZIP   :
                $ar =~ /.+?\.(?:tbz2?|tar\.bz2?)$/i ? TBZ   :
                $ar =~ /.+?\.bz2$/i                 ? BZ2   :
                $ar =~ /.+?\.Z$/                    ? Z     :
                $ar =~ /.+?\.lzma$/                 ? LZMA  :
                '';

        }

        bless $parsed, $class;

        ### don't know what type of file it is 
        ### XXX this *has* to be an object call, not a package call
        return $parsed->_error(loc("Cannot determine file type for '%1'",
                                $parsed->{archive} )) unless $parsed->{type};
        return $parsed;
    }
}

sub extract {
    my $self = shift;
    my %hash = @_;

    ### reset error messages
    $self->_error_msg( [] );
    $self->_error_msg_long( [] );

    my $to;
    my $tmpl = {
        to  => { default => '.', store => \$to }
    };

    check( $tmpl, \%hash ) or return;

    ### so 'to' could be a file or a dir, depending on whether it's a .gz 
    ### file, or basically anything else.
    ### so, check that, then act accordingly.
    ### set an accessor specifically so _gunzip can know what file to extract
    ### to.
    my $dir;
    {   ### a foo.gz file
        if( $self->is_gz or $self->is_bz2 or $self->is_Z or $self->is_lzma ) {
    
            my $cp = $self->archive; $cp =~ s/\.(?:gz|bz2?|Z|lzma)$//i;
        
            ### to is a dir?
            if ( -d $to ) {
                $dir = $to; 
                $self->_gunzip_to( basename($cp) );

            ### then it's a filename
            } else {
                $dir = dirname($to);
                $self->_gunzip_to( basename($to) );
            }

        ### not a foo.gz file
        } else {
            $dir = $to;
        }
    }

    ### make the dir if it doesn't exist ###
    unless( -d $dir ) {
        eval { mkpath( $dir ) };

        return $self->_error(loc("Could not create path '%1': %2", $dir, $@))
            if $@;
    }

    ### get the current dir, to restore later ###
    my $cwd = cwd();

    my $ok = 1;
    EXTRACT: {

        ### chdir to the target dir ###
        unless( chdir $dir ) {
            $self->_error(loc("Could not chdir to '%1': %2", $dir, $!));
            $ok = 0; last EXTRACT;
        }

        ### set files to an empty array ref, so there's always an array
        ### ref IN the accessor, to avoid errors like:
        ### Can't use an undefined value as an ARRAY reference at
        ### ../lib/Archive/Extract.pm line 742. (rt #19815)
        $self->files( [] );

        ### find out the dispatch methods needed for this type of 
        ### archive. Do a $self->is_XXX to figure out the type, then
        ### get the hashref with bin + pure perl dispatchers.
        my ($map) = map { $Mapping->{$_} } grep { $self->$_ } keys %$Mapping;

        ### add pure perl extractor if allowed & add bin extractor if allowed
        my @methods;
        push @methods, $map->{'pp'}  if $_ALLOW_PURE_PERL;
        push @methods, $map->{'bin'} if $_ALLOW_BIN;
        
        ### reverse it if we prefer bin extractors
        @methods = reverse @methods if $PREFER_BIN;

        my($na, $fail);
        for my $method (@methods) {
            print "# Extracting with ->$method\n" if $DEBUG;
        
            my $rv = $self->$method;
            
            ### a positive extraction
            if( $rv and $rv ne METHOD_NA ) {
                print "# Extraction succeeded\n" if $DEBUG;
                $self->_extractor($method);
                last;
            
            ### method is not available
            } elsif ( $rv and $rv eq METHOD_NA ) {               
                print "# Extraction method not available\n" if $DEBUG;
                $na++;                
            } else {
                print "# Extraction method failed\n" if $DEBUG;
                $fail++;
            }                
        }

        ### warn something went wrong if we didn't get an extractor
        unless( $self->_extractor ) {
            my $diag = $fail ? loc("Extract failed due to errors") :
                       $na   ? loc("Extract failed; no extractors available") :
                       '';
                       
            $self->_error($diag);
            $ok = 0;
        }                   
    }

    ### and chdir back ###
    unless( chdir $cwd ) {
        $self->_error(loc("Could not chdir back to start dir '%1': %2'",
                            $cwd, $!));
    }

    return $ok;
}

sub types { return @Types }

### quick check methods ###
sub is_tgz  { return $_[0]->type eq TGZ }
sub is_tar  { return $_[0]->type eq TAR }
sub is_gz   { return $_[0]->type eq GZ  }
sub is_zip  { return $_[0]->type eq ZIP }
sub is_tbz  { return $_[0]->type eq TBZ }
sub is_bz2  { return $_[0]->type eq BZ2 }
sub is_Z    { return $_[0]->type eq Z   }
sub is_lzma { return $_[0]->type eq LZMA }

### paths to commandline tools ###
sub bin_gzip        { return $PROGRAMS->{'gzip'}    if $PROGRAMS->{'gzip'}  }
sub bin_unzip       { return $PROGRAMS->{'unzip'}   if $PROGRAMS->{'unzip'} }
sub bin_tar         { return $PROGRAMS->{'tar'}     if $PROGRAMS->{'tar'}   }
sub bin_bunzip2     { return $PROGRAMS->{'bunzip2'} if $PROGRAMS->{'bunzip2'} }
sub bin_uncompress  { return $PROGRAMS->{'uncompress'} 
                                                 if $PROGRAMS->{'uncompress'} }
sub bin_unlzma      { return $PROGRAMS->{'unlzma'}  if $PROGRAMS->{'unlzma'} }

sub have_old_bunzip2 {
    my $self = shift;

    ### no bunzip2? no old bunzip2 either :)
    return unless $self->bin_bunzip2;

    ### if we can't run this, we can't be sure if it's too old or not    
    ### XXX stupid stupid stupid bunzip2 doesn't understand --version
    ### is not a request to extract data:
    ### $ bunzip2 --version
    ### bzip2, a block-sorting file compressor.  Version 1.0.2, 30-Dec-2001.
    ### [...]
    ### bunzip2: I won't read compressed data from a terminal.
    ### bunzip2: For help, type: `bunzip2 --help'.
    ### $ echo $?
    ### 1
    ### HATEFUL!
    
    ### double hateful: bunzip2 --version also hangs if input is a pipe
    ### See #32370: Archive::Extract will hang if stdin is a pipe [+PATCH]
    ### So, we have to provide *another* argument which is a fake filename,
    ### just so it wont try to read from stdin to print its version..
    ### *sigh*
    ### Even if the file exists, it won't clobber or change it.
    my $buffer;
    scalar run( 
         command => [$self->bin_bunzip2, '--version', 'NoSuchFile'],
         verbose => 0,
         buffer  => \$buffer
    );

    ### no output
    return unless $buffer;
    
    my ($version) = $buffer =~ /version \s+ (\d+)/ix;

    return 1 if $version < 1;
    return;
}

#################################
#
# Untar code
#
#################################

### annoying issue with (gnu) tar on win32, as illustrated by this
### bug: https://rt.cpan.org/Ticket/Display.html?id=40138
### which shows that (gnu) tar will interpret a file name with a :
### in it as a remote file name, so C:\tmp\foo.txt is interpreted
### as a remote shell, and the extract fails.
{   my @ExtraTarFlags;
    if( ON_WIN32 and my $cmd = __PACKAGE__->bin_tar ) {

        ### if this is gnu tar we are running, we need to use --force-local
        push @ExtraTarFlags, '--force-local' if `$cmd --version` =~ /gnu tar/i;
    }        


    ### use /bin/tar to extract ###
    sub _untar_bin {
        my $self = shift;
    
        ### check for /bin/tar ###
        ### check for /bin/gzip if we need it ###
        ### if any of the binaries are not available, return NA
        {   my $diag =  not $self->bin_tar ? 
                            loc("No '%1' program found", '/bin/tar') :
                        $self->is_tgz && !$self->bin_gzip ? 
                            loc("No '%1' program found", '/bin/gzip') :
                        $self->is_tbz && !$self->bin_bunzip2 ?
                            loc("No '%1' program found", '/bin/bunzip2') :
                        '';
                        
            if( $diag ) {
                $self->_error( $diag );
                return METHOD_NA;
            }
        }        
    
        ### XXX figure out how to make IPC::Run do this in one call --
        ### currently i don't know how to get output of a command after a pipe
        ### trapped in a scalar. Mailed barries about this 5th of june 2004.
    
        ### see what command we should run, based on whether
        ### it's a .tgz or .tar
    
        ### XXX solaris tar and bsdtar are having different outputs
        ### depending whether you run with -x or -t
        ### compensate for this insanity by running -t first, then -x
        {    my $cmd = 
                $self->is_tgz ? [$self->bin_gzip, '-cdf', $self->archive, '|', 
                                 $self->bin_tar, '-tf', '-'] :
                $self->is_tbz ? [$self->bin_bunzip2, '-cd', $self->archive, '|',                             
                                 $self->bin_tar, '-tf', '-'] :
                [$self->bin_tar, @ExtraTarFlags, '-tf', $self->archive];
    
            ### run the command 
            ### newer versions of 'tar' (1.21 and up) now print record size
            ### to STDERR as well if v OR t is given (used to be both). This 
            ### is a 'feature' according to the changelog, so we must now only
            ### inspect STDOUT, otherwise, failures like these occur:
            ### nntp.perl.org/group/perl.cpan.testers/2009/02/msg3230366.html
            my $buffer  = '';
            my @out     = run(  command => $cmd,
                                buffer  => \$buffer,
                                verbose => $DEBUG );

            ### command was unsuccessful            
            unless( $out[0] ) { 
                return $self->_error(loc(
                                "Error listing contents of archive '%1': %2",
                                $self->archive, $buffer ));
            }
    
            ### no buffers available?
            if( !IPC::Cmd->can_capture_buffer and !$buffer ) {
                $self->_error( $self->_no_buffer_files( $self->archive ) );
            
            } else {
                ### if we're on solaris we /might/ be using /bin/tar, which has
                ### a weird output format... we might also be using
                ### /usr/local/bin/tar, which is gnu tar, which is perfectly
                ### fine... so we have to do some guessing here =/
                my @files = map { chomp;
                              !ON_SOLARIS ? $_
                                          : (m|^ x \s+  # 'xtract' -- sigh
                                                (.+?),  # the actual file name
                                                \s+ [\d,.]+ \s bytes,
                                                \s+ [\d,.]+ \s tape \s blocks
                                            |x ? $1 : $_);
    
                        ### only STDOUT, see above. Sometims, extra whitespace
                        ### is present, so make sure we only pick lines with
                        ### a length
                        } grep { length } map { split $/, $_ } @{$out[3]};     
    
                ### store the files that are in the archive ###
                $self->files(\@files);
            }
        }
    
        ### now actually extract it ###
        {   my $cmd = 
                $self->is_tgz ? [$self->bin_gzip, '-cdf', $self->archive, '|',
                                 $self->bin_tar, '-xf', '-'] :
                $self->is_tbz ? [$self->bin_bunzip2, '-cd', $self->archive, '|',                             
                                 $self->bin_tar, '-xf', '-'] :
                [$self->bin_tar, @ExtraTarFlags, '-xf', $self->archive];
    
            my $buffer = '';
            unless( scalar run( command => $cmd,
                                buffer  => \$buffer,
                                verbose => $DEBUG )
            ) {
                return $self->_error(loc("Error extracting archive '%1': %2",
                                $self->archive, $buffer ));
            }
    
            ### we might not have them, due to lack of buffers
            if( $self->files ) {
                ### now that we've extracted, figure out where we extracted to
                my $dir = $self->__get_extract_dir( $self->files );
        
                ### store the extraction dir ###
                $self->extract_path( $dir );
            }
        }
    
        ### we got here, no error happened
        return 1;
    }
}


### use archive::tar to extract ###
sub _untar_at {
    my $self = shift;

    ### Loading Archive::Tar is going to set it to 1, so make it local
    ### within this block, starting with its initial value. Whatever
    ### Achive::Tar does will be undone when we return.
    ###
    ### Also, later, set $Archive::Tar::WARN to $Archive::Extract::WARN
    ### so users don't have to even think about this variable. If they
    ### do, they still get their set value outside of this call.
    local $Archive::Tar::WARN = $Archive::Tar::WARN;
   
    ### we definitely need Archive::Tar, so load that first
    {   my $use_list = { 'Archive::Tar' => '0.0' };

        unless( can_load( modules => $use_list ) ) {

            $self->_error(loc("You do not have '%1' installed - " .
                              "Please install it as soon as possible.",
                              'Archive::Tar'));
    
            return METHOD_NA;
        }
    }

    ### we might pass it a filehandle if it's a .tbz file..
    my $fh_to_read = $self->archive;

    ### we will need Compress::Zlib too, if it's a tgz... and IO::Zlib
    ### if A::T's version is 0.99 or higher
    if( $self->is_tgz ) {
        my $use_list = { 'Compress::Zlib' => '0.0' };
           $use_list->{ 'IO::Zlib' } = '0.0'
                if $Archive::Tar::VERSION >= '0.99';

        unless( can_load( modules => $use_list ) ) {
            my $which = join '/', sort keys %$use_list;

            $self->_error(loc(
                "You do not have '%1' installed - Please ".
                "install it as soon as possible.", $which)
            );
            
            return METHOD_NA;
        }

    } elsif ( $self->is_tbz ) {
        my $use_list = { 'IO::Uncompress::Bunzip2' => '0.0' };
        unless( can_load( modules => $use_list ) ) {
            $self->_error(loc(
                "You do not have '%1' installed - Please " .
                "install it as soon as possible.", 
                'IO::Uncompress::Bunzip2')
            );
            
            return METHOD_NA;
        }

        my $bz = IO::Uncompress::Bunzip2->new( $self->archive ) or
            return $self->_error(loc("Unable to open '%1': %2",
                            $self->archive,
                            $IO::Uncompress::Bunzip2::Bunzip2Error));

        $fh_to_read = $bz;
    }

    ### $Archive::Tar::WARN is 1 by default in Archive::Tar, but we've
    ### localized $Archive::Tar::WARN already.
    $Archive::Tar::WARN = $Archive::Extract::WARN;

    my $tar = Archive::Tar->new();

    ### only tell it it's compressed if it's a .tgz, as we give it a file
    ### handle if it's a .tbz
    unless( $tar->read( $fh_to_read, ( $self->is_tgz ? 1 : 0 ) ) ) {
        return $self->_error(loc("Unable to read '%1': %2", $self->archive,
                                    $Archive::Tar::error));
    }

    ### workaround to prevent Archive::Tar from setting uid, which
    ### is a potential security hole. -autrijus
    ### have to do it here, since A::T needs to be /loaded/ first ###
    {   no strict 'refs'; local $^W;

        ### older versions of archive::tar <= 0.23
        *Archive::Tar::chown = sub {};
    }

    ### for version of Archive::Tar > 1.04
    local $Archive::Tar::CHOWN = 0;

    {   local $^W;  # quell 'splice() offset past end of array' warnings
                    # on older versions of A::T

        ### older archive::tar always returns $self, return value slightly
        ### fux0r3d because of it.
        $tar->extract()
            or return $self->_error(loc("Unable to extract '%1': %2",
                                    $self->archive, $Archive::Tar::error ));
    }

    my @files   = $tar->list_files;
    my $dir     = $self->__get_extract_dir( \@files );

    ### store the files that are in the archive ###
    $self->files(\@files);

    ### store the extraction dir ###
    $self->extract_path( $dir );

    ### check if the dir actually appeared ###
    return 1 if -d $self->extract_path;

    ### no dir, we failed ###
    return $self->_error(loc("Unable to extract '%1': %2",
                                $self->archive, $Archive::Tar::error ));
}

#################################
#
# Gunzip code
#
#################################

sub _gunzip_bin {
    my $self = shift;

    ### check for /bin/gzip -- we need it ###
    unless( $self->bin_gzip ) {
        $self->_error(loc("No '%1' program found", '/bin/gzip'));
        return METHOD_NA;
    }

    my $fh = FileHandle->new('>'. $self->_gunzip_to) or
        return $self->_error(loc("Could not open '%1' for writing: %2",
                            $self->_gunzip_to, $! ));

    my $cmd = [ $self->bin_gzip, '-cdf', $self->archive ];

    my $buffer;
    unless( scalar run( command => $cmd,
                        verbose => $DEBUG,
                        buffer  => \$buffer )
    ) {
        return $self->_error(loc("Unable to gunzip '%1': %2",
                                    $self->archive, $buffer));
    }

    ### no buffers available?
    if( !IPC::Cmd->can_capture_buffer and !$buffer ) {
        $self->_error( $self->_no_buffer_content( $self->archive ) );
    }

    print $fh $buffer if defined $buffer;

    close $fh;

    ### set what files where extract, and where they went ###
    $self->files( [$self->_gunzip_to] );
    $self->extract_path( File::Spec->rel2abs(cwd()) );

    return 1;
}

sub _gunzip_cz {
    my $self = shift;

    my $use_list = { 'Compress::Zlib' => '0.0' };
    unless( can_load( modules => $use_list ) ) {
        $self->_error(loc("You do not have '%1' installed - Please " .
                    "install it as soon as possible.", 'Compress::Zlib'));
        return METHOD_NA;
    }

    my $gz = Compress::Zlib::gzopen( $self->archive, "rb" ) or
                return $self->_error(loc("Unable to open '%1': %2",
                            $self->archive, $Compress::Zlib::gzerrno));

    my $fh = FileHandle->new('>'. $self->_gunzip_to) or
        return $self->_error(loc("Could not open '%1' for writing: %2",
                            $self->_gunzip_to, $! ));

    my $buffer;
    $fh->print($buffer) while $gz->gzread($buffer) > 0;
    $fh->close;

    ### set what files where extract, and where they went ###
    $self->files( [$self->_gunzip_to] );
    $self->extract_path( File::Spec->rel2abs(cwd()) );

    return 1;
}

#################################
#
# Uncompress code
#
#################################

sub _uncompress_bin {
    my $self = shift;

    ### check for /bin/gzip -- we need it ###
    unless( $self->bin_uncompress ) {
        $self->_error(loc("No '%1' program found", '/bin/uncompress'));
        return METHOD_NA;
    }

    my $fh = FileHandle->new('>'. $self->_gunzip_to) or
        return $self->_error(loc("Could not open '%1' for writing: %2",
                            $self->_gunzip_to, $! ));

    my $cmd = [ $self->bin_uncompress, '-c', $self->archive ];

    my $buffer;
    unless( scalar run( command => $cmd,
                        verbose => $DEBUG,
                        buffer  => \$buffer )
    ) {
        return $self->_error(loc("Unable to uncompress '%1': %2",
                                    $self->archive, $buffer));
    }

    ### no buffers available?
    if( !IPC::Cmd->can_capture_buffer and !$buffer ) {
        $self->_error( $self->_no_buffer_content( $self->archive ) );
    }

    print $fh $buffer if defined $buffer;

    close $fh;

    ### set what files where extract, and where they went ###
    $self->files( [$self->_gunzip_to] );
    $self->extract_path( File::Spec->rel2abs(cwd()) );

    return 1;
}


#################################
#
# Unzip code
#
#################################


sub _unzip_bin {
    my $self = shift;

    ### check for /bin/gzip if we need it ###
    unless( $self->bin_unzip ) {
        $self->_error(loc("No '%1' program found", '/bin/unzip'));
        return METHOD_NA;
    }        

    ### first, get the files.. it must be 2 different commands with 'unzip' :(
    {   ### on VMS, capital letter options have to be quoted. This is
        ### peported by John Malmberg on P5P Tue 21 Aug 2007 05:05:11 
        ### Subject: [patch@31735]Archive Extract fix on VMS.
        my $opt = ON_VMS ? '"-Z"' : '-Z';
        my $cmd = [ $self->bin_unzip, $opt, '-1', $self->archive ];
	
        my $buffer = '';
        unless( scalar run( command => $cmd,
                            verbose => $DEBUG,
                            buffer  => \$buffer )
        ) {
            return $self->_error(loc("Unable to unzip '%1': %2",
                                        $self->archive, $buffer));
        }

        ### no buffers available?
        if( !IPC::Cmd->can_capture_buffer and !$buffer ) {
            $self->_error( $self->_no_buffer_files( $self->archive ) );

        } else {
            $self->files( [split $/, $buffer] );
        }
    }

    ### now, extract the archive ###
    {   my $cmd = [ $self->bin_unzip, '-qq', '-o', $self->archive ];

        my $buffer;
        unless( scalar run( command => $cmd,
                            verbose => $DEBUG,
                            buffer  => \$buffer )
        ) {
            return $self->_error(loc("Unable to unzip '%1': %2",
                                        $self->archive, $buffer));
        }

        if( scalar @{$self->files} ) {
            my $files   = $self->files;
            my $dir     = $self->__get_extract_dir( $files );

            $self->extract_path( $dir );
        }
    }

    return 1;
}

sub _unzip_az {
    my $self = shift;

    my $use_list = { 'Archive::Zip' => '0.0' };
    unless( can_load( modules => $use_list ) ) {
        $self->_error(loc("You do not have '%1' installed - Please " .
                      "install it as soon as possible.", 'Archive::Zip'));
        return METHOD_NA;                      
    }

    my $zip = Archive::Zip->new();

    unless( $zip->read( $self->archive ) == &Archive::Zip::AZ_OK ) {
        return $self->_error(loc("Unable to read '%1'", $self->archive));
    }

    my @files;
    
    
    ### Address: #43278: Explicitly tell Archive::Zip where to put the files:
    ### "In my BackPAN indexing, Archive::Zip was extracting things
    ### in my script's directory instead of the current working directory.
    ### I traced this back through Archive::Zip::_asLocalName which
    ### eventually calls File::Spec::Win32::rel2abs which on Windows might
    ### call Cwd::getdcwd. getdcwd returns the wrong directory in my
    ### case, even though I think I'm on the same drive.
    ### 
    ### To fix this, I pass the optional second argument to
    ### extractMember using the cwd from Archive::Extract." --bdfoy

    ## store cwd() before looping; calls to cwd() can be expensive, and
    ### it won't change during the loop
    my $extract_dir = cwd();
    
    ### have to extract every member individually ###
    for my $member ($zip->members) {
        push @files, $member->{fileName};

        ### file to extact to, to avoid the above problem
        my $to = File::Spec->catfile( $extract_dir, $member->{fileName} );
        
        unless( $zip->extractMember($member, $to) == &Archive::Zip::AZ_OK ) {
            return $self->_error(loc("Extraction of '%1' from '%2' failed",
                        $member->{fileName}, $self->archive ));
        }
    }

    my $dir = $self->__get_extract_dir( \@files );

    ### set what files where extract, and where they went ###
    $self->files( \@files );
    $self->extract_path( File::Spec->rel2abs($dir) );

    return 1;
}

sub __get_extract_dir {
    my $self    = shift;
    my $files   = shift || [];

    return unless scalar @$files;

    my($dir1, $dir2);
    for my $aref ( [ \$dir1, 0 ], [ \$dir2, -1 ] ) {
        my($dir,$pos) = @$aref;

        ### add a catdir(), so that any trailing slashes get
        ### take care of (removed)
        ### also, a catdir() normalises './dir/foo' to 'dir/foo';
        ### which was the problem in bug #23999
        my $res = -d $files->[$pos]
                    ? File::Spec->catdir( $files->[$pos], '' )
                    : File::Spec->catdir( dirname( $files->[$pos] ) ); 

        $$dir = $res;
    }

    ### if the first and last dir don't match, make sure the 
    ### dirname is not set wrongly
    my $dir;
 
    ### dirs are the same, so we know for sure what the extract dir is
    if( $dir1 eq $dir2 ) {
        $dir = $dir1;
    
    ### dirs are different.. do they share the base dir?
    ### if so, use that, if not, fall back to '.'
    } else {
        my $base1 = [ File::Spec->splitdir( $dir1 ) ]->[0];
        my $base2 = [ File::Spec->splitdir( $dir2 ) ]->[0];
        
        $dir = File::Spec->rel2abs( $base1 eq $base2 ? $base1 : '.' ); 
    }        

    return File::Spec->rel2abs( $dir );
}

#################################
#
# Bunzip2 code
#
#################################

sub _bunzip2_bin {
    my $self = shift;

    ### check for /bin/gzip -- we need it ###
    unless( $self->bin_bunzip2 ) {
        $self->_error(loc("No '%1' program found", '/bin/bunzip2'));
        return METHOD_NA;
    }        

    my $fh = FileHandle->new('>'. $self->_gunzip_to) or
        return $self->_error(loc("Could not open '%1' for writing: %2",
                            $self->_gunzip_to, $! ));
    
    ### guard against broken bunzip2. See ->have_old_bunzip2()
    ### for details
    if( $self->have_old_bunzip2 and $self->archive !~ /\.bz2$/i ) {
        return $self->_error(loc("Your bunzip2 version is too old and ".
                                 "can only extract files ending in '%1'",
                                 '.bz2'));
    }

    my $cmd = [ $self->bin_bunzip2, '-cd', $self->archive ];

    my $buffer;
    unless( scalar run( command => $cmd,
                        verbose => $DEBUG,
                        buffer  => \$buffer )
    ) {
        return $self->_error(loc("Unable to bunzip2 '%1': %2",
                                    $self->archive, $buffer));
    }

    ### no buffers available?
    if( !IPC::Cmd->can_capture_buffer and !$buffer ) {
        $self->_error( $self->_no_buffer_content( $self->archive ) );
    }
    
    print $fh $buffer if defined $buffer;

    close $fh;

    ### set what files where extract, and where they went ###
    $self->files( [$self->_gunzip_to] );
    $self->extract_path( File::Spec->rel2abs(cwd()) );

    return 1;
}

### using cz2, the compact versions... this we use mainly in archive::tar
### extractor..
# sub _bunzip2_cz1 {
#     my $self = shift;
# 
#     my $use_list = { 'IO::Uncompress::Bunzip2' => '0.0' };
#     unless( can_load( modules => $use_list ) ) {
#         return $self->_error(loc("You do not have '%1' installed - Please " .
#                         "install it as soon as possible.",
#                         'IO::Uncompress::Bunzip2'));
#     }
# 
#     my $bz = IO::Uncompress::Bunzip2->new( $self->archive ) or
#                 return $self->_error(loc("Unable to open '%1': %2",
#                             $self->archive,
#                             $IO::Uncompress::Bunzip2::Bunzip2Error));
# 
#     my $fh = FileHandle->new('>'. $self->_gunzip_to) or
#         return $self->_error(loc("Could not open '%1' for writing: %2",
#                             $self->_gunzip_to, $! ));
# 
#     my $buffer;
#     $fh->print($buffer) while $bz->read($buffer) > 0;
#     $fh->close;
# 
#     ### set what files where extract, and where they went ###
#     $self->files( [$self->_gunzip_to] );
#     $self->extract_path( File::Spec->rel2abs(cwd()) );
# 
#     return 1;
# }

sub _bunzip2_bz2 {
    my $self = shift;

    my $use_list = { 'IO::Uncompress::Bunzip2' => '0.0' };
    unless( can_load( modules => $use_list ) ) {
        $self->_error(loc("You do not have '%1' installed - Please " .
                          "install it as soon as possible.",
                          'IO::Uncompress::Bunzip2'));
        return METHOD_NA;                          
    }

    IO::Uncompress::Bunzip2::bunzip2($self->archive => $self->_gunzip_to)
        or return $self->_error(loc("Unable to uncompress '%1': %2",
                            $self->archive,
                            $IO::Uncompress::Bunzip2::Bunzip2Error));

    ### set what files where extract, and where they went ###
    $self->files( [$self->_gunzip_to] );
    $self->extract_path( File::Spec->rel2abs(cwd()) );

    return 1;
}


#################################
#
# unlzma code
#
#################################

sub _unlzma_bin {
    my $self = shift;

    ### check for /bin/unlzma -- we need it ###
    unless( $self->bin_unlzma ) {
        $self->_error(loc("No '%1' program found", '/bin/unlzma'));
        return METHOD_NA;
    }        

    my $fh = FileHandle->new('>'. $self->_gunzip_to) or
        return $self->_error(loc("Could not open '%1' for writing: %2",
                            $self->_gunzip_to, $! ));

    my $cmd = [ $self->bin_unlzma, '-c', $self->archive ];

    my $buffer;
    unless( scalar run( command => $cmd,
                        verbose => $DEBUG,
                        buffer  => \$buffer )
    ) {
        return $self->_error(loc("Unable to unlzma '%1': %2",
                                    $self->archive, $buffer));
    }

    ### no buffers available?
    if( !IPC::Cmd->can_capture_buffer and !$buffer ) {
        $self->_error( $self->_no_buffer_content( $self->archive ) );
    }

    print $fh $buffer if defined $buffer;

    close $fh;

    ### set what files where extract, and where they went ###
    $self->files( [$self->_gunzip_to] );
    $self->extract_path( File::Spec->rel2abs(cwd()) );

    return 1;
}

sub _unlzma_cz {
    my $self = shift;

    my $use_list = { 'Compress::unLZMA' => '0.0' };
    unless( can_load( modules => $use_list ) ) {
        $self->_error(loc("You do not have '%1' installed - Please " .
                    "install it as soon as possible.", 'Compress::unLZMA'));
        return METHOD_NA;                    
    }

    my $fh = FileHandle->new('>'. $self->_gunzip_to) or
        return $self->_error(loc("Could not open '%1' for writing: %2",
                            $self->_gunzip_to, $! ));

    my $buffer;
    $buffer = Compress::unLZMA::uncompressfile( $self->archive );
    unless ( defined $buffer ) {
        return $self->_error(loc("Could not unlzma '%1': %2",
                                    $self->archive, $@));
    }

    print $fh $buffer if defined $buffer;

    close $fh;

    ### set what files where extract, and where they went ###
    $self->files( [$self->_gunzip_to] );
    $self->extract_path( File::Spec->rel2abs(cwd()) );

    return 1;
}

#################################
#
# Error code
#
#################################

sub _error {
    my $self    = shift;
    my $error   = shift;
    my $lerror  = Carp::longmess($error);

    push @{$self->_error_msg},      $error;
    push @{$self->_error_msg_long}, $lerror;
    
    ### set $Archive::Extract::WARN to 0 to disable printing
    ### of errors
    if( $WARN ) {
        carp $DEBUG ? $lerror : $error;
    }

    return;
}

sub error {
    my $self = shift;

    ### make sure we have a fallback aref
    my $aref = do { 
        shift() 
            ? $self->_error_msg_long 
            : $self->_error_msg 
    } || [];
   
    return join $/, @$aref;
}

sub _no_buffer_files {
    my $self = shift;
    my $file = shift or return;
    return loc("No buffer captured, unable to tell ".
               "extracted files or extraction dir for '%1'", $file);
}

sub _no_buffer_content {
    my $self = shift;
    my $file = shift or return;
    return loc("No buffer captured, unable to get content for '%1'", $file);
}
1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

