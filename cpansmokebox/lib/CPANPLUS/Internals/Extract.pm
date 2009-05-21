package CPANPLUS::Internals::Extract;

use strict;

use CPANPLUS::Error;
use CPANPLUS::Internals::Constants;

use File::Spec                  ();
use File::Basename              ();
use Archive::Extract;
use IPC::Cmd                    qw[run];
use Params::Check               qw[check];
use Module::Load::Conditional   qw[can_load check_install];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';

local $Params::Check::VERBOSE = 1;

sub _extract {
    my $self = shift;
    my $conf = $self->configure_object;
    my %hash = @_;
    
    local $Params::Check::ALLOW_UNKNOWN = 1;
    
    my( $mod, $verbose, $force );
    my $tmpl = {
        force       => { default => $conf->get_conf('force'),   
                            store => \$force },
        verbose     => { default => $conf->get_conf('verbose'), 
                            store => \$verbose },
        prefer_bin  => { default => $conf->get_conf('prefer_bin') },
        extractdir  => { default => $conf->get_conf('extractdir') },
        module      => { required => 1, allow => IS_MODOBJ, store => \$mod },
        perl        => { default => $^X },
    };
    
    my $args = check( $tmpl, \%hash ) or return;
    
    ### did we already extract it ? ###
    my $loc = $mod->status->extract();
    
    if( $loc && !$force ) {
        msg(loc("Already extracted '%1' to '%2'. ".
                "Won't extract again without force",
                $mod->module, $loc), $verbose);
        return $loc;
    }

    ### did we already fetch the file? ###
    my $file = $mod->status->fetch();
    unless( -s $file ) {
        error( loc( "File '%1' has zero size: cannot extract", $file ) );    
        return;
    }

    ### the dir to extract to ###
    my $to =    $args->{'extractdir'} ||
                File::Spec->catdir(
                        $conf->get_conf('base'),
                        $self->_perl_version( perl => $args->{'perl'} ),
                        $conf->_get_build('moddir'),
                );
 
    ### delegate to Archive::Extract ###
    ### set up some flags for archive::extract ###
    local $Archive::Extract::PREFER_BIN = $args->{'prefer_bin'};
    local $Archive::Extract::DEBUG      = $conf->get_conf('debug');
    local $Archive::Extract::WARN       = $verbose;

    my $ae = Archive::Extract->new( archive => $file );

    unless( $ae->extract( to => $to ) ) {
        error( loc( "Unable to extract '%1' to '%2': %3",
                    $file, $to, $ae->error ) );
        return;
    }
    
    ### if ->files is not filled, we dont know what the hell was
    ### extracted.. try to offer a suggestion and bail :(
    unless ( $ae->files ) {
        error( loc( "'%1' was not able to determine extracted ".
                    "files from the archive. Instal '%2' and ensure ".
                    "it works properly and try again",
                    $ae->is_zip ? 'Archive::Zip' : 'Archive::Tar' ) );
        return;                    
    }                    
    
    
    ### print out what files we extracted ###  
    msg(loc("Extracted '%1'",$_),$verbose) for @{$ae->files};  
    
    ### set them all to be +w for the owner, so we don't get permission
    ### denied for overwriting files that are just +r
    
    ### this is to rigurous -- just change to +w for the owner [cpan #13358] 
    #chmod 0755, map { File::Spec->rel2abs( File::Spec->catdir($to, $_) ) }
    #            @{$ae->files};
    
    for my $file ( @{$ae->files} ) { 
        my $path = File::Spec->rel2abs( File::Spec->catfile($to, $file) );
    
        $self->_mode_plus_w( file => $path );
    }
    
    ### check the return value for the extracted path ###
    ### Make an educated guess if we didn't get an extract_path
    ### back
    ### XXX apparently some people make their own dists and they 
    ### pack up '.' which means the leading directory is '.' 
    ### and only the second directory is the actual module directory
    ### so, we'll have to check if our educated guess exists first, 
    ### then see if the extract path works.. and if nothing works...
    ### well, then we really don't know.

    my $dir;
    for my $try (
        File::Spec->rel2abs( 
            ### _safe_path must be called before catdir because catdir on 
            ### VMS currently will not handle the extra dots in the directories.
            File::Spec->catdir( $self->_safe_path( path => $to ) ,  
                                $self->_safe_path( path =>
                                             $mod->package_name .'-'. 
                                             $mod->package_version
        ) ) ) ,
        File::Spec->rel2abs( $ae->extract_path ),
    ) {
        ($dir = $try) && last if -d $try;
    }
                                            
    ### test if the dir exists ###
    unless( $dir && -d $dir ) {
        error(loc("Unable to determine extract dir for '%1'",$mod->module));
        return;
    
    } else {    
        msg(loc("Extracted '%1' to '%2'", $mod->module, $dir), $verbose);
        
        ### register where we extracted the files to,
        ### also store what files were extracted
        $mod->status->extract( $dir ); 
        $mod->status->files( $ae->files );
    }
      
    ### also, figure out what kind of install we're dealing with ###
    $mod->get_installer_type();

    return $mod->status->extract();
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
