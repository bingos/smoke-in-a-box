package CPANPLUS::Dist::MM;

use warnings;
use strict;
use vars    qw[@ISA $STATUS];
use base    'CPANPLUS::Dist::Base';

use CPANPLUS::Internals::Constants;
use CPANPLUS::Internals::Constants::Report;
use CPANPLUS::Error;
use FileHandle;
use Cwd;

use IPC::Cmd                    qw[run];
use Params::Check               qw[check];
use File::Basename              qw[dirname];
use Module::Load::Conditional   qw[can_load check_install];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';

local $Params::Check::VERBOSE = 1;

### check if the format is available ###
sub format_available {
    my $dist = shift;
  
    ### we might be called as $class->format_available =/
    require CPANPLUS::Internals;
    my $cb   = CPANPLUS::Internals->_retrieve_id( 
                    CPANPLUS::Internals->_last_id );
    my $conf = $cb->configure_object;
  
    my $mod = "ExtUtils::MakeMaker";
    unless( can_load( modules => { $mod => 0.0 } ) ) {
        error( loc( "You do not have '%1' -- '%2' not available",
                    $mod, __PACKAGE__ ) ); 
        return;
    }
    
    for my $pgm ( qw[make] ) {
        unless( $conf->get_program( $pgm ) ) { 
            error(loc(
                "You do not have '%1' in your path -- '%2' not available\n" .
                "Please check your config entry for '%1'", 
                $pgm, __PACKAGE__ , $pgm
            )); 
            return;
        }
    }

    return 1;     
}

sub init {
    my $dist    = shift;
    my $status  = $dist->status;
   
    $status->mk_accessors(qw[makefile make test created installed uninstalled
                             bin_make _prepare_args _create_args _install_args]
                        );
    
    return 1;
}    

sub prepare {
    ### just in case you already did a create call for this module object
    ### just via a different dist object
    my $dist = shift;
    my $self = $dist->parent;
    
    ### we're also the cpan_dist, since we don't need to have anything
    ### prepared 
    $dist    = $self->status->dist_cpan if      $self->status->dist_cpan;     
    $self->status->dist_cpan( $dist )   unless  $self->status->dist_cpan;    

    my $cb   = $self->parent;
    my $conf = $cb->configure_object;
    my %hash = @_;

    my $dir;
    unless( $dir = $self->status->extract ) {
        error( loc( "No dir found to operate on!" ) );
        return;
    }
    
    my $args;
    my( $force, $verbose, $perl, @mmflags, $prereq_target, $prereq_format,
        $prereq_build );
    {   local $Params::Check::ALLOW_UNKNOWN = 1;
        my $tmpl = {
            perl            => {    default => $^X, store => \$perl },
            makemakerflags  => {    default =>
                                        $conf->get_conf('makemakerflags') || '',
                                    store => \$mmflags[0] },
            force           => {    default => $conf->get_conf('force'), 
                                    store   => \$force },
            verbose         => {    default => $conf->get_conf('verbose'), 
                                    store   => \$verbose },
            prereq_target   => {    default => '', store => \$prereq_target }, 
            prereq_format   => {    default => '',
                                    store   => \$prereq_format },   
            prereq_build    => {    default => 0, store => \$prereq_build },     
        };                                            

        $args = check( $tmpl, \%hash ) or return;
    }
    
    
    ### maybe we already ran a create on this object? ###
    return 1 if $dist->status->prepared && !$force;
        
    ### store the arguments, so ->install can use them in recursive loops ###
    $dist->status->_prepare_args( $args );
    
    ### chdir to work directory ###
    my $orig = cwd();
    unless( $cb->_chdir( dir => $dir ) ) {
        error( loc( "Could not chdir to build directory '%1'", $dir ) );
        return;
    }
    
    my $fail; 
    RUN: {

        ### we resolve 'configure requires' here, so we can run the 'perl
        ### Makefile.PL' command
        ### XXX for tests: mock f_c_r to something that *can* resolve and
        ### something that *doesnt* resolve. Check the error log for ok
        ### on this step or failure
        ### XXX make a seperate tarball to test for this scenario: simply
        ### containing a makefile.pl/build.pl for test purposes?
        {   my $configure_requires = $dist->find_configure_requires;     
            my $ok = $dist->_resolve_prereqs(
                            format          => $prereq_format,
                            verbose         => $verbose,
                            prereqs         => $configure_requires,
                            target          => $prereq_target,
                            force           => $force,
                            prereq_build    => $prereq_build,
                    );    
    
            unless( $ok ) {
           
                #### use $dist->flush to reset the cache ###
                error( loc( "Unable to satisfy '%1' for '%2' " .
                            "-- aborting install", 
                            'configure_requires', $self->module ) );    
                $dist->status->prepared(0);
                $fail++; 
                last RUN;
            } 
            ### end of prereq resolving ###
        }
        


        ### don't run 'perl makefile.pl' again if there's a makefile already 
        if( -e MAKEFILE->() && (-M MAKEFILE->() < -M $dir) && !$force ) {
            msg(loc("'%1' already exists, not running '%2 %3' again ".
                    " unless you force",
                    MAKEFILE->(), $perl, MAKEFILE_PL->() ), $verbose );
            
        } else {
            unless( -e MAKEFILE_PL->() ) {
                msg(loc("No '%1' found - attempting to generate one",
                        MAKEFILE_PL->() ), $verbose );
                        
                $dist->write_makefile_pl( 
                            verbose => $verbose, 
                            force   => $force 
                        );
                
                ### bail out if there's no makefile.pl ###
                unless( -e MAKEFILE_PL->() ) {
                    error( loc( "Could not find '%1' - cannot continue", 
                                MAKEFILE_PL->() ) );
        
                    ### mark that we screwed up ###
                    $dist->status->makefile(0);
                    $fail++; last RUN;
                }
            }    
    
            ### you can turn off running this verbose by changing
            ### the config setting below, although it is really not
            ### recommended
            my $run_verbose = $verbose || 
                              $conf->get_conf('allow_build_interactivity') ||
                              0;
    
            ### this makes MakeMaker use defaults if possible, according
            ### to schwern. See ticket 8047 for details.
            local $ENV{PERL_MM_USE_DEFAULT} = 1 unless $run_verbose; 
    
            ### turn off our PERL5OPT so no modules from CPANPLUS::inc get
            ### included in the makefile.pl -- it should build without
            ### also, modules that run in taint mode break if we leave
            ### our code ref in perl5opt
            ### XXX we've removed the ENV settings from cp::inc, so only need
            ### to reset the @INC
            #local $ENV{PERL5OPT} = CPANPLUS::inc->original_perl5opt || ''; 
    
            ### make sure it's a string, so that mmflags that have more than
            ### one key value pair are passed as is, rather than as:
            ### perl Makefile.PL "key=val key=>val"
            
            
            #### XXX this needs to be the absolute path to the Makefile.PL
            ### since cpanp-run-perl uses 'do' to execute the file, and do()
            ### checks your @INC.. so, if there's _another_ makefile.pl in
            ### your @INC, it will execute that one...
            my $makefile_pl = MAKEFILE_PL->( $cb->_safe_path( path => $dir ) );
            
            ### setting autoflush to true fixes issue from rt #8047
            ### XXX this means that we need to keep the path to CPANPLUS
            ### in @INC, stopping us from resolving dependencies on CPANPLUS
            ### at bootstrap time properly.

            ### XXX this fails under ipc::run due to the extra quotes,
            ### but it works in ipc::open3. however, ipc::open3 doesn't work
            ### on win32/cygwin. XXX TODO get a windows box and sort this out
            # my $cmd =  qq[$perl -MEnglish -le ] . 
            #            QUOTE_PERL_ONE_LINER->(
            #                qq[\$OUTPUT_AUTOFLUSH++,do(q($makefile_pl))]
            #            ) 
            #            . $mmflags;

            # my $flush = OPT_AUTOFLUSH;
            # my $cmd     = "$perl $flush $makefile_pl $mmflags";

            my $run_perl    = $conf->get_program('perlwrapper');
            my $cmd         = [$perl, $run_perl, $makefile_pl, @mmflags];

            ### set ENV var to tell underlying code this is what we're
            ### executing.
            my $captured; 
            my $rv = do {
                my $env = ENV_CPANPLUS_IS_EXECUTING;
                local $ENV{$env} = $makefile_pl;
                scalar run( command => $cmd,
                            buffer  => \$captured,
                            verbose => $run_verbose, # may be interactive   
                        );
            };
    
            unless( $rv ) {
                error( loc( "Could not run '%1 %2': %3 -- cannot continue",
                            $perl, MAKEFILE_PL->(), $captured ) );
                
                $dist->status->makefile(0);
                $fail++; last RUN;
            }

            ### put the output on the stack, don't print it
            msg( $captured, 0 );
        }
        
        ### so, nasty feature in Module::Build, that when a Makefile.PL
        ### is a disguised Build.PL, it generates a Build file, not a
        ### Makefile. this breaks everything :( see rt bug #19741
        if( not -e MAKEFILE->( $dir ) and -e BUILD_PL->( $dir ) ) {
            error(loc(
                    "We just ran '%1' without errors, but no '%2' is ".
                    "present. However, there is a '%3' file, so this may ".
                    "be related to bug #19741 in %4, which describes a ".
                    "fake '%5' which generates a '%6' file instead of a '%7'. ".
                    "You could try to work around this issue by setting '%8' ".
                    "to false and trying again. This will attempt to use the ".
                    "'%9' instead.",
                    "$^X ".MAKEFILE_PL->(), MAKEFILE->(), BUILD_PL->(),
                    'Module::Build', MAKEFILE_PL->(), 'Build', MAKEFILE->(),
                    'prefer_makefile', BUILD_PL->()
            ));           
            
            $fail++, last RUN;
        }
        
        ### if we got here, we managed to make a 'makefile' ###
        $dist->status->makefile( MAKEFILE->($dir) );               
        
        ### start resolving prereqs ###
        my $prereqs = $self->status->prereqs;
       
        ### a hashref of prereqs on success, undef on failure ###
        $prereqs    ||= $dist->_find_prereqs( 
                                    verbose => $verbose,
                                    file    => $dist->status->makefile 
                                );
        
        unless( $prereqs ) {
            error( loc( "Unable to scan '%1' for prereqs", 
                        $dist->status->makefile ) );

            $fail++; last RUN;
        }
    }
   
	unless( $cb->_chdir( dir => $orig ) ) {
        error( loc( "Could not chdir back to start dir '%1'", $orig ) );
    }   
   
    ### save where we wrote this stuff -- same as extract dir in normal
    ### installer circumstances
    $dist->status->distdir( $self->status->extract );
   
    return $dist->status->prepared( $fail ? 0 : 1);
}

sub _find_prereqs {
    my $dist = shift;
    my $self = $dist->parent;
    my $cb   = $self->parent;
    my $conf = $cb->configure_object;
    my %hash = @_;

    my ($verbose, $file);
    my $tmpl = {
        verbose => { default => $conf->get_conf('verbose'), store => \$verbose },
        file    => { required => 1, allow => FILE_READABLE, store => \$file },
    };
    
    my $args = check( $tmpl, \%hash ) or return;      
    
    my $fh = FileHandle->new();
    unless( $fh->open( $file ) ) {
        error( loc( "Cannot open '%1': %2", $file, $! ) );
        return;
    }
    
    my %p;
    while( local $_ = <$fh> ) {
        my ($found) = m|^[\#]\s+PREREQ_PM\s+=>\s+(.+)|;         
        
        next unless $found;
        
        while( $found =~ m/(?:\s)([\w\:]+)=>(?:q\[(.*?)\],?|undef)/g ) {
            if( defined $p{$1} ) {
                msg(loc("Warning: PREREQ_PM mentions '%1' more than once. " .
                        "Last mention wins.", $1 ), $verbose );
            }
            
            $p{$1} = $cb->_version_to_number(version => $2);                  
        }
        last;
    }

    my $href = $cb->_callbacks->filter_prereqs->( $cb, \%p );

    $self->status->prereqs( $href );
    
    ### just to make sure it's not the same reference ###
    return { %$href };                              
}     

sub create {
    ### just in case you already did a create call for this module object
    ### just via a different dist object
    my $dist = shift;
    my $self = $dist->parent;
    
    ### we're also the cpan_dist, since we don't need to have anything
    ### prepared 
    $dist    = $self->status->dist_cpan if      $self->status->dist_cpan;     
    $self->status->dist_cpan( $dist )   unless  $self->status->dist_cpan;    

    my $cb   = $self->parent;
    my $conf = $cb->configure_object;
    my %hash = @_;

    my $dir;
    unless( $dir = $self->status->extract ) {
        error( loc( "No dir found to operate on!" ) );
        return;
    }
    
    my $args;
    my( $force, $verbose, $make, $makeflags, $skiptest, $prereq_target, $perl, 
        @mmflags, $prereq_format, $prereq_build);
    {   local $Params::Check::ALLOW_UNKNOWN = 1;
        my $tmpl = {
            perl            => {    default => $^X, store => \$perl },
            force           => {    default => $conf->get_conf('force'), 
                                    store   => \$force },
            verbose         => {    default => $conf->get_conf('verbose'), 
                                    store   => \$verbose },
            make            => {    default => $conf->get_program('make'), 
                                    store   => \$make },
            makeflags       => {    default => $conf->get_conf('makeflags'), 
                                    store   => \$makeflags },
            skiptest        => {    default => $conf->get_conf('skiptest'), 
                                    store   => \$skiptest },
            prereq_target   => {    default => '', store => \$prereq_target }, 
            ### don't set the default prereq format to 'makemaker' -- wrong!
            prereq_format   => {    #default => $self->status->installer_type,
                                    default => '',
                                    store   => \$prereq_format },   
            prereq_build    => {    default => 0, store => \$prereq_build },                                    
        };                                            

        $args = check( $tmpl, \%hash ) or return;
    }
    
    ### maybe we already ran a create on this object?
    ### make sure we add to include path again, just in case we came from
    ### ->save_state, at which point we need to restore @INC/$PERL5LIB
    if( $dist->status->created && !$force ) {
        $self->add_to_includepath;
        return 1;
    }        
    
    ### store the arguments, so ->install can use them in recursive loops ###
    $dist->status->_create_args( $args );
    
    unless( $dist->status->prepared ) {
        error( loc( "You have not successfully prepared a '%2' distribution ".
                    "yet -- cannot create yet", __PACKAGE__ ) );
        return;
    }
    
    
    ### chdir to work directory ###
    my $orig = cwd();
    unless( $cb->_chdir( dir => $dir ) ) {
        error( loc( "Could not chdir to build directory '%1'", $dir ) );
        return;
    }
    
    my $fail; my $prereq_fail; my $test_fail;
    RUN: {
        ### this will set the directory back to the start
        ### dir, so we must chdir /again/           
        my $ok = $dist->_resolve_prereqs(
                            format          => $prereq_format,
                            verbose         => $verbose,
                            prereqs         => $self->status->prereqs,
                            target          => $prereq_target,
                            force           => $force,
                            prereq_build    => $prereq_build,
                    );
        
        unless( $cb->_chdir( dir => $dir ) ) {
            error( loc( "Could not chdir to build directory '%1'", $dir ) );
            return;
        }       
                  
        unless( $ok ) {
       
            #### use $dist->flush to reset the cache ###
            error( loc( "Unable to satisfy prerequisites for '%1' " .
                        "-- aborting install", $self->module ) );    
            $dist->status->make(0);
            $fail++; $prereq_fail++;
            last RUN;
        } 
        ### end of prereq resolving ###    
        
        my $captured;

        ### 'make' section ###    
        if( -d BLIB->($dir) && (-M BLIB->($dir) < -M $dir) && !$force ) {
            msg(loc("Already ran '%1' for this module [%2] -- " .
                    "not running again unless you force", 
                    $make, $self->module ), $verbose );
        } else {
            unless(scalar run(  command => [$make, $makeflags],
                                buffer  => \$captured,
                                verbose => $verbose ) 
            ) {
                error( loc( "MAKE failed: %1 %2", $!, $captured ) );
                $dist->status->make(0);
                $fail++; last RUN;
            }
            
            ### put the output on the stack, don't print it
            msg( $captured, 0 );

            $dist->status->make(1);

            ### add this directory to your lib ###
            $self->add_to_includepath();
            
            ### dont bail out here, there's a conditional later on
            #last RUN if $skiptest;
        }
        
        ### 'make test' section ###                                           
        unless( $skiptest ) {

            ### turn off our PERL5OPT so no modules from CPANPLUS::inc get
            ### included in make test -- it should build without
            ### also, modules that run in taint mode break if we leave
            ### our code ref in perl5opt
            ### XXX CPANPLUS::inc functionality is now obsolete.
            #local $ENV{PERL5OPT} = CPANPLUS::inc->original_perl5opt || '';

            ### you can turn off running this verbose by changing
            ### the config setting below, although it is really not 
            ### recommended
            my $run_verbose =   
                        $verbose || 
                        $conf->get_conf('allow_build_interactivity') ||
                        0;

            ### XXX need to add makeflags here too? 
            ### yes, but they should really be split out -- see bug #4143
            if( scalar run( 
                        command => [$make, 'test', $makeflags],
                        buffer  => \$captured,
                        verbose => $run_verbose,
            ) ) {
                ### tests might pass because it doesn't have any tests defined
                ### log this occasion non-verbosely, so our test reporter can
                ### pick up on this
                if ( NO_TESTS_DEFINED->( $captured ) ) {
                    msg( NO_TESTS_DEFINED->( $captured ), 0 )
                } else {
                    msg( loc( "MAKE TEST passed: %2", $captured ), $verbose );
                }
            
                $dist->status->test(1);
            } else {
                error( loc( "MAKE TEST failed: %1 %2", $!, $captured ) );
            
                ### send out error report here? or do so at a higher level?
                ### --higher level --kane.
                $dist->status->test(0);
               
                ### mark specifically *test* failure.. so we dont
                ### send success on force...
                $test_fail++;
                
                if( !$force and !$cb->_callbacks->proceed_on_test_failure->(
                                      $self, $captured ) 
                ) {
                    $fail++; last RUN;     
                }
            }
        }
    } #</RUN>
      
    unless( $cb->_chdir( dir => $orig ) ) {
        error( loc( "Could not chdir back to start dir '%1'", $orig ) );
    }  
    
    ### send out test report?
    ### only do so if the failure is this module, not its prereq
    if( $conf->get_conf('cpantest') and not $prereq_fail) {
        $cb->_send_report( 
            module  => $self,
            failed  => $test_fail || $fail,
            buffer  => CPANPLUS::Error->stack_as_string,
            verbose => $verbose,
            force   => $force,
        ) or error(loc("Failed to send test report for '%1'",
                    $self->module ) );
    }            
            
    return $dist->status->created( $fail ? 0 : 1);
} 

sub install {

    ### just in case you did the create with ANOTHER dist object linked
    ### to the same module object
    my $dist = shift();
    my $self = $dist->parent;
    $dist    = $self->status->dist_cpan if $self->status->dist_cpan;       
   
    my $cb   = $self->parent;
    my $conf = $cb->configure_object;
    my %hash = @_;
    
    
    unless( $dist->status->created ) {
        error(loc("You have not successfully created a '%2' distribution yet " .
                  "-- cannot install yet", __PACKAGE__ ));
        return;
    }
 
    my $dir;
    unless( $dir = $self->status->extract ) {
        error( loc( "No dir found to operate on!" ) );
        return;
    }
    
    my $args;
    my($force,$verbose,$make,$makeflags);
    {   local $Params::Check::ALLOW_UNKNOWN = 1;
        my $tmpl = {
            force       => {    default => $conf->get_conf('force'), 
                                store   => \$force },
            verbose     => {    default => $conf->get_conf('verbose'), 
                                store   => \$verbose },
            make        => {    default => $conf->get_program('make'), 
                                store   => \$make },
            makeflags   => {    default => $conf->get_conf('makeflags'), 
                                store   => \$makeflags },
        };      
    
        $args = check( $tmpl, \%hash ) or return;
    }

    ### value set and false -- means failure ###
    if( defined $self->status->installed && 
        !$self->status->installed && !$force 
    ) {
        error( loc( "Module '%1' has failed to install before this session " .
                    "-- aborting install", $self->module ) );
        return;
    }

            
    $dist->status->_install_args( $args );
    
    my $orig = cwd();
    unless( $cb->_chdir( dir => $dir ) ) {
        error( loc( "Could not chdir to build directory '%1'", $dir ) );
        return;
    }
    
    my $fail; my $captured;
    
    ### 'make install' section ###
    ### XXX need makeflags here too? 
    ### yes, but they should really be split out.. see bug #4143
    my $cmd     = [$make, 'install', $makeflags];
    my $sudo    = $conf->get_program('sudo');
    unshift @$cmd, $sudo if $sudo and $>;

    $cb->flush('lib');
    unless(scalar run(  command => $cmd,
                        verbose => $verbose,
                        buffer  => \$captured,
    ) ) {                   
        error( loc( "MAKE INSTALL failed: %1 %2", $!, $captured ) );
        $fail++; 
    }       

    ### put the output on the stack, don't print it
    msg( $captured, 0 );
    
    unless( $cb->_chdir( dir => $orig ) ) {
        error( loc( "Could not chdir back to start dir '%1'", $orig ) );
    }   
    
    return $dist->status->installed( $fail ? 0 : 1 );
    
}

sub write_makefile_pl {
    ### just in case you already did a call for this module object
    ### just via a different dist object
    my $dist = shift;
    my $self = $dist->parent;
    $dist    = $self->status->dist_cpan if      $self->status->dist_cpan;     
    $self->status->dist_cpan( $dist )   unless  $self->status->dist_cpan;    
 
    my $cb   = $self->parent;
    my $conf = $cb->configure_object;
    my %hash = @_;

    my $dir;
    unless( $dir = $self->status->extract ) {
        error( loc( "No dir found to operate on!" ) );
        return;
    }
    
    my ($force, $verbose);
    my $tmpl = {
        force           => {    default => $conf->get_conf('force'),   
                                store => \$force },
        verbose         => {    default => $conf->get_conf('verbose'), 
                                store => \$verbose },   
    };                                          

    my $args = check( $tmpl, \%hash ) or return;    
    
    my $file = MAKEFILE_PL->($dir);
    if( -s $file && !$force ) {
        msg(loc("Already created '%1' - not doing so again without force", 
                $file ), $verbose );
        return 1;
    }     

    ### due to a bug with AS perl 5.8.4 built 810 (and maybe others)
    ### opening files with content in them already does nasty things;
    ### seek to pos 0 and then print, but not truncating the file
    ### bug reported to activestate on 19 sep 2004:
    ### http://bugs.activestate.com/show_bug.cgi?id=34051
    unlink $file if $force;

    my $fh = new FileHandle;
    unless( $fh->open( ">$file" ) ) {
        error( loc( "Could not create file '%1': %2", $file, $! ) );
        return;
    }
    
    my $mf      = MAKEFILE_PL->();
    my $name    = $self->module;
    my $version = $self->version;
    my $author  = $self->author->author;
    my $href    = $self->status->prereqs;
    my $prereqs = join ",\n", map { 
                                (' ' x 25) . "'$_'\t=> '$href->{$_}'" 
                            } keys %$href;  
    $prereqs ||= ''; # just in case there are none;                         
                             
    print $fh qq|
    ### Auto-generated $mf by CPANPLUS ###
    
    use ExtUtils::MakeMaker;
    
    WriteMakefile(
        NAME        => '$name',
        VERSION     => '$version',
        AUTHOR      => '$author',
        PREREQ_PM   => {
$prereqs                       
                    },
    );
    \n|;   
    
    $fh->close;
    return 1;
}                         
        
sub dist_dir {
    ### just in case you already did a call for this module object
    ### just via a different dist object
    my $dist = shift;
    my $self = $dist->parent;
    $dist    = $self->status->dist_cpan if      $self->status->dist_cpan;     
    $self->status->dist_cpan( $dist )   unless  $self->status->dist_cpan;    
 
    my $cb   = $self->parent;
    my $conf = $cb->configure_object;
    my %hash = @_;
    
    my $make; my $verbose;
    {   local $Params::Check::ALLOW_UNKNOWN = 1;
        my $tmpl = {
            make    => {    default => $conf->get_program('make'),
                                    store => \$make },                 
            verbose => {    default => $conf->get_conf('verbose'), 
                                    store   => \$verbose },
        };  
    
        check( $tmpl, \%hash ) or return;    
    }


    my $dir;
    unless( $dir = $self->status->extract ) {
        error( loc( "No dir found to operate on!" ) );
        return;
    }
    
    ### chdir to work directory ###
    my $orig = cwd();
    unless( $cb->_chdir( dir => $dir ) ) {
        error( loc( "Could not chdir to build directory '%1'", $dir ) );
        return;
    }

    my $fail; my $distdir;
    TRY: {    
        $dist->prepare( @_ ) or (++$fail, last TRY);


        my $captured;             
            unless(scalar run(  command => [$make, 'distdir'],
                            buffer  => \$captured,
                            verbose => $verbose ) 
        ) {
            error( loc( "MAKE DISTDIR failed: %1 %2", $!, $captured ) );
            ++$fail, last TRY;
        }

        ### /path/to/Foo-Bar-1.2/Foo-Bar-1.2
        $distdir = File::Spec->catdir( $dir, $self->package_name . '-' .
                                                $self->package_version );

        unless( -d $distdir ) {
            error(loc("Do not know where '%1' got created", 'distdir'));
            ++$fail, last TRY;
        }
    }

    unless( $cb->_chdir( dir => $orig ) ) {
        error( loc( "Could not chdir to start directory '%1'", $orig ) );
        return;
    }

    return if $fail;
    return $distdir;
}    


1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
