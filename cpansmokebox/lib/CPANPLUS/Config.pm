package CPANPLUS::Config;

use strict;
use warnings;

use base 'Object::Accessor';

use base 'CPANPLUS::Internals::Utils';

use Config;
use File::Spec;
use Module::Load;
use CPANPLUS;
use CPANPLUS::Error;
use CPANPLUS::Internals::Constants;

use File::Basename              qw[dirname];
use IPC::Cmd                    qw[can_run];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';
use Module::Load::Conditional   qw[check_install];


### BAH! you can't have POD interleaved with a hash
### declaration.. so declare every entry seperatedly :(
my $Conf = {
    '_fetch' => {
        'blacklist' => [ 'ftp' ],
    },
    
    ### _source, _build and _mirror are supposed to be static
    ### no changes should be needed unless pause/cpan changes
    '_source' => {
        'hosts'             => 'MIRRORED.BY',
        'auth'              => '01mailrc.txt.gz',
        'stored'            => 'sourcefiles',
        'dslip'             => '03modlist.data.gz',
        'update'            => '86400',
        'mod'               => '02packages.details.txt.gz',
        'custom_index'      => 'packages.txt',
    },
    '_build' => {
        'plugins'           => 'plugins',
        'moddir'            => 'build',
        'startdir'          => '',
        'distdir'           => 'dist',
        'autobundle'        => 'autobundle',
        'autobundle_prefix' => 'Snapshot',
        'autdir'            => 'authors',
        'install_log_dir'   => 'install-logs',
        'custom_sources'    => 'custom-sources',
        'sanity_check'      => 1,
    },
    '_mirror' => {
        'base'              => 'authors/id/',
        'auth'              => 'authors/01mailrc.txt.gz',
        'dslip'             => 'modules/03modlist.data.gz',
        'mod'               => 'modules/02packages.details.txt.gz'
    },
};

    ### default host list
    $Conf->{'conf'}->{'hosts'} = [
            {
                'scheme' => 'ftp',
                'path' => '/pub/CPAN/',
                'host' => 'ftp.cpan.org'
            },
            {
                'scheme' => 'http',
                'path' => '/',
                'host' => 'www.cpan.org'
            },
            {
                'scheme' => 'ftp',
                'path' => '/pub/CPAN/',
                'host' => 'ftp.nl.uu.net'
            },
            {
                'scheme' => 'ftp',
                'path' => '/pub/CPAN/',
                'host' => 'cpan.valueclick.com'
            },
            {
                'scheme' => 'ftp',
                'path' => '/pub/languages/perl/CPAN/',
                'host' => 'ftp.funet.fi'
            }
        ];
        
        $Conf->{'conf'}->{'allow_build_interactivity'} = 1;

       $Conf->{'conf'}->{'base'} = File::Spec->catdir(
                                        __PACKAGE__->_home_dir, DOT_CPANPLUS );

        $Conf->{'conf'}->{'buildflags'} = '';

        $Conf->{'conf'}->{'cpantest'} = 0;

        $Conf->{'conf'}->{'cpantest_mx'} = '';

        $Conf->{'conf'}->{'debug'} = 0;

        $Conf->{'conf'}->{'dist_type'} = '';

        $Conf->{'conf'}->{'email'} = DEFAULT_EMAIL;
        
        ### this addresses #32248 which requests a possibillity to
        ### turn off custom sources
        $Conf->{'conf'}->{'enable_custom_sources'} = 1;

        $Conf->{'conf'}->{'extractdir'} = '';

        $Conf->{'conf'}->{'fetchdir'} = '';

        $Conf->{'conf'}->{'flush'} = 1;

        $Conf->{'conf'}->{'force'} = 0;

        $Conf->{'conf'}->{'lib'} = [];

        $Conf->{'conf'}->{'makeflags'} = '';

        $Conf->{'conf'}->{'makemakerflags'} = '';

        $Conf->{'conf'}->{'md5'} = ( 
                            check_install( module => 'Digest::MD5' ) ? 1 : 0 );

        $Conf->{'conf'}->{'no_update'} = 0;

        $Conf->{'conf'}->{'passive'} = 1;

        $Conf->{'conf'}->{'prefer_makefile'} = 1;

        $Conf->{'conf'}->{'prereqs'} = PREREQ_ASK;

        $Conf->{'conf'}->{'shell'} = 'CPANPLUS::Shell::Default';

        $Conf->{'conf'}->{'show_startup_tip'} = 1;

        $Conf->{'conf'}->{'signature'} = do {
            check_install( module => 'Module::Signature', version => '0.06' )
            and ( can_run('gpg') || 
                  check_install(module => 'Crypt::OpenPGP')
            );
        } ? 1 : 0;

        $Conf->{'conf'}->{'skiptest'} = 0;

       $Conf->{'conf'}->{'storable'} = 
                        ( check_install( module => 'Storable' ) ? 1 : 0 );

        $Conf->{'conf'}->{'timeout'} = 300;

        $Conf->{'conf'}->{'verbose'} = $ENV{PERL5_CPANPLUS_VERBOSE} || 0;

        $Conf->{'conf'}->{'write_install_logs'} = 1;

        $Conf->{'conf'}->{'source_engine'} = DEFAULT_SOURCE_ENGINE; 

        $Conf->{'conf'}->{'cpantest_reporter_args'} = {};

    ### Paths get stripped of whitespace on win32 in the constructor
    ### sudo gets emptied if there's no need for it in the constructor

        $Conf->{'program'}->{'editor'} = do {
            $ENV{'EDITOR'}  || $ENV{'VISUAL'} ||
            can_run('vi')   || can_run('pico')
        };

        $Conf->{'program'}->{'make'} = 
            can_run($Config{'make'}) || can_run('make');

        $Conf->{'program'}->{'pager'} = 
            $ENV{'PAGER'} || can_run('less') || can_run('more');

        ### no one uses this feature anyway, and it's only working for EU::MM
        ### and not for module::build
        #'perl'      => '',

        $Conf->{'program'}->{'shell'} = $^O eq 'MSWin32' 
                                        ? $ENV{COMSPEC} 
                                        : $ENV{SHELL};

        $Conf->{'program'}->{'sudo'} = do {
            ### let's assume you dont need sudo,
            ### unless one of the below criteria tells us otherwise
            my $sudo = undef;
            
            ### you're a normal user, you might need sudo
            if( $> ) {
    
                ### check for all install dirs!
                ### you have write permissions to the installdir,
                ### you don't need sudo
                if( -w $Config{'installsitelib'} && -w $Config{'installsitebin'} ) {                    
                    
                    ### installsiteman3dir is a 5.8'ism.. don't check
                    ### it on 5.6.x...            
                    if( defined $Config{'installsiteman3dir'} ) {
                        $sudo = -w $Config{'installsiteman3dir'} 
                            ? undef
                            : can_run('sudo');
                    } else {
                        $sudo = undef;
                    }

                ### you have PERL_MM_OPT set to some alternate
                ### install place. You probably have write permissions
                ### to that
                } elsif ( $ENV{'PERL_MM_OPT'} and 
                          $ENV{'PERL_MM_OPT'} =~ /INSTALL|LIB|PREFIX/
                ) {
                    $sudo = undef;

                ### you probably don't have write permissions
                } else {                
                    $sudo = can_run('sudo');
                }
            }  
            
            ### and return the value
            $sudo;
        };

        ### perlwrapper that allows us to turn on autoflushing                        
        $Conf->{'program'}->{'perlwrapper'} = sub { 
            my $name = 'cpanp-run-perl';

            my @bins = do{
                require Config;
                my $ver  = $Config::Config{version};
                
                ### if we are running with 'versiononly' enabled,
                ### all binaries will have the perlversion appended
                ### ie, cpanp will become cpanp5.9.5
                ### so prefer the versioned binary in that case
                $Config::Config{versiononly}
                        ? ($name.$ver, $name)
                        : ($name, $name.$ver);
            };

            ### patch from Steve Hay Fri 29 Jun 2007 14:26:02 GMT+02:00
            ### Msg-Id: <4684FA5A.7030506@uk.radan.com>
            ### look for files with a ".bat" extension as well on Win32
            @bins = map { $_, "$_.bat" } @bins if $^O eq 'MSWin32';

            my $path;
            BIN: for my $bin (@bins) {
                
                ### parallel to your cpanp/cpanp-boxed
                my $maybe = File::Spec->rel2abs(
                                File::Spec->catfile( dirname($0), $bin )
                            );        
                $path = $maybe and last BIN if -f $maybe;
        
                ### parallel to your CPANPLUS.pm:
                ### $INC{cpanplus}/../bin/cpanp-run-perl
                $maybe = File::Spec->rel2abs(
                            File::Spec->catfile( 
                                dirname($INC{'CPANPLUS.pm'}),
                                '..',   # lib dir
                                'bin',  # bin dir
                                $bin,   # script
                            )
                         );
                $path = $maybe and last BIN if -f $maybe;
                         
                ### you installed CPANPLUS in a custom prefix,
                ### so go paralel to /that/. PREFIX=/tmp/cp
                ### would put cpanp-run-perl in /tmp/cp/bin and
                ### CPANPLUS.pm in
                ### /tmp/cp/lib/perl5/site_perl/5.8.8
                $maybe = File::Spec->rel2abs(
                            File::Spec->catfile( 
                                dirname( $INC{'CPANPLUS.pm'} ),
                                '..', '..', '..', '..', # 4x updir
                                'bin',                  # bin dir
                                $bin,                   # script
                            )
                         );
                $path = $maybe and last BIN if -f $maybe;

                ### in your path -- take this one last, the
                ### previous two assume extracted tarballs
                ### or user installs
                ### note that we don't use 'can_run' as it's
                ### not an executable, just a wrapper...
                ### prefer anything that's found in the path paralel to your $^X
                for my $dir (File::Spec->rel2abs( dirname($^X) ),
                             split(/\Q$Config::Config{path_sep}\E/, $ENV{PATH}),
                             File::Spec->curdir, 
                ) {             

                    ### On VMS the path could be in UNIX format, and we
                    ### currently need it to be in VMS format
                    $dir = VMS::Filespec::vmspath($dir) if ON_VMS;

                    $maybe = File::Spec->catfile( $dir, $bin );
                    $path = $maybe and last BIN if -f $maybe;
                }
            }          
                
            ### we should have a $path by now ideally, if so return it
            return $path if defined $path;
            
            ### if not, warn about it and give sensible default.
            ### XXX try to be a no-op instead then.. 
            ### cross your fingers...
            ### pass '-P' to perl: "run program through C 
            ### preprocessor before compilation"
            ### XXX using -P actually changes the way some Makefile.PLs
            ### are executed, so don't do that... --kane
            error(loc(
                "Could not find the '%1' binary in your path".
                "--this may be a problem.\n".
                "Please locate this program and set ".
                "your '%2' config entry to its path.\n".
                "From the default shell, you can do this by typing:\n\n".
                "  %3\n".
                "  %4\n",
                $name, 'perlwrapper', 
                's program perlwrapper FULL_PATH_TO_CPANP_RUN_PERL',
                's save'
             ));                                        
             return '';
        }->();
        
sub new {
    my $class   = shift;
    my $obj     = $class->SUPER::new;

    $obj->mk_accessors( keys %$Conf );

    for my $acc ( keys %$Conf ) {
        my $subobj = Object::Accessor->new;
        $subobj->mk_accessors( keys %{$Conf->{$acc}} );

        ### read in all the settings from the sub accessors;
        for my $subacc ( $subobj->ls_accessors ) {
            $subobj->$subacc( $Conf->{$acc}->{$subacc} );
        }

        ### now store it in the parent object
        $obj->$acc( $subobj );
    }
    
    $obj->_clean_up_paths;
    
    ### shut up IPC::Cmd warning about not findin IPC::Run on win32
    $IPC::Cmd::WARN = 0;
    
    return $obj;
}

sub _clean_up_paths {
    my $self = shift;

    ### clean up paths if we are on win32
    if( $^O eq 'MSWin32' ) {
        for my $pgm ( $self->program->ls_accessors ) {
            my $path = $self->program->$pgm;

            ### paths with whitespace needs to be shortened
            ### for shell outs.
            if ($path and $path =~ /\s+/) {
                my($prog, $args);

                ### patch from Steve Hay, 13nd of June 2007
                ### msg-id: <467012A4.6060705@uk.radan.com>
                ### windows directories are not allowed to end with 
                ### a space, so any occurrence of '\w\s+/\w+' means
                ### we're dealing with arguments, not directory
                ### names.
                if ($path =~ /^(.*?)(\s+\/.*$)/) {
                    ($prog, $args) = ($1, $2);
                
                ### otherwise, there are no arguments
                } else {
                    ($prog, $args) = ($path, '');
                }
                
                $prog = Win32::GetShortPathName( $prog );
                $self->program->$pgm( $prog . $args );
            }
        }
    }

    return 1;
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
