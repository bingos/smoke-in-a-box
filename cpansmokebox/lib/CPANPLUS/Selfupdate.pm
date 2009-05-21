package CPANPLUS::Selfupdate;

use strict;
use Params::Check               qw[check];
use IPC::Cmd                    qw[can_run];
use CPANPLUS::Error             qw[error msg];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';

use CPANPLUS::Internals::Constants;

$Params::Check::VERBOSE = 1;

### a config has describing our deps etc
{

    my $Modules = {
        dependencies => {
            'File::Fetch'               => '0.15_02', # lynx & 404 handling
            'File::Spec'                => '0.82',
            'IPC::Cmd'                  => '0.36', # 5.6.2 compat: 2-arg open
            'Locale::Maketext::Simple'  => '0.01',
            'Log::Message'              => '0.01',
            'Module::Load'              => '0.10',
            'Module::Load::Conditional' => '0.28', # returns dir for loaded
                                                   # modules
            'version'                   => '0.73', # needed for M::L::C
                                                   # addresses #24630 and 
                                                   # #24675
                                                   # Address ~0 overflow issue
            'Params::Check'             => '0.22',
            'Package::Constants'        => '0.01',
            'Term::UI'                  => '0.18', # option parsing
            'Test::Harness'             => '2.62', # due to bug #19505
                                                   # only 2.58 and 2.60 are bad
            'Test::More'                => '0.47', # to run our tests
            'Archive::Extract'          => '0.16', # ./Dir bug fix
            'Archive::Tar'              => '1.23',
            'IO::Zlib'                  => '1.04', # needed for Archive::Tar
            'Object::Accessor'          => '0.34', # mk_aliases support
            'Module::CoreList'          => '2.09',
            'Module::Pluggable'         => '2.4',
            'Module::Loaded'            => '0.01',
            'Parse::CPAN::Meta'         => '0.02', # config_requires support
            'ExtUtils::Install'         => '1.42', # uninstall outside @INC
        },
    
        features => {
            # config_key_name => [
            #     sub { } to list module key/value pairs
            #     sub { } to check if feature is enabled
            # ]
            prefer_makefile => [
                sub {
                    my $cb = shift;
                    $cb->configure_object->get_conf('prefer_makefile') 
                        ? { }
                        : { 'CPANPLUS::Dist::Build' => '0.24'  };
                },
                sub { return 1 },   # always enabled
            ],            
            cpantest        => [
                { 'Test::Reporter'  => '1.34',
                  'YAML::Tiny'      => '0.0'
                },
                sub { 
                    my $cb = shift;
                    return $cb->configure_object->get_conf('cpantest');
                },
            ],                
            dist_type => [
                sub { 
                    my $cb      = shift;
                    my $dist    = $cb->configure_object->get_conf('dist_type');
                    return { $dist => '0.0' } if $dist;
                    return;
                },            
                sub { 
                    my $cb = shift;
                    return $cb->configure_object->get_conf('dist_type');
                },
            ],

            md5 => [
                {
                    'Digest::MD5'   => '0.0',
                },            
                sub { 
                    my $cb = shift;
                    return $cb->configure_object->get_conf('md5');
                },
            ],
            shell => [
                sub { 
                    my $cb      = shift;
                    my $dist    = $cb->configure_object->get_conf('shell');
                    
                    ### we bundle these shells, so don't bother having a dep
                    ### on them... If we don't do this, CPAN.pm actually detects
                    ### a recursive dependency and breaks (see #26077).
                    ### This is not an issue for CPANPLUS itself, it handles
                    ### it smartly.
                    return if $dist eq SHELL_DEFAULT or $dist eq SHELL_CLASSIC;
                    return { $dist => '0.0' } if $dist;
                    return;
                },            
                sub { return 1 },
            ],                
            signature => [
                sub {
                    my $cb      = shift;
                    return {
                        'Module::Signature' => '0.06',
                    } if can_run('gpg');
                    ### leave this out -- Crypt::OpenPGP is fairly
                    ### painful to install, and broken on some platforms
                    ### so we'll just always fall back to gpg. It may
                    ### issue a warning or 2, but that's about it.
                    ### this change due to this ticket: #26914
                    # and $cb->configure_object->get_conf('prefer_bin');

                    return { 
                        'Crypt::OpenPGP'    => '0.0', 
                        'Module::Signature' => '0.06',
                    };
                },            
                sub {
                    my $cb = shift;
                    return $cb->configure_object->get_conf('signature');
                },
            ],
            storable => [
                { 'Storable' => '0.0' },         
                sub { 
                    my $cb = shift;
                    return $cb->configure_object->get_conf('storable');
                },
            ],
            sqlite_backend => [
                {   'DBIx::Simple' => '0.0',
                    'DBD::SQLite'  => '0.0',
                },
                sub {
                    my $cb   = shift;
                    my $conf = $cb->configure_object;
                    return $conf->get_conf('source_engine') 
                        eq 'CPANPLUS::Internals::Source::SQLite'
                },                        
            ],                    
        },
        core => {
            'CPANPLUS' => '0.0',
        },
    };

    sub _get_config { return $Modules }
}

sub new {
    my $class = shift;
    my $cb    = shift or return;
    return bless sub { $cb }, $class;
}    


{   ### cache to find the relevant modules
    my $cache = {
        core 
            => sub { my $self = shift;
                     core => [ $self->list_core_modules ]   },
 
        dependencies        
            => sub { my $self = shift;
                     dependencies => [ $self->list_core_dependencies ] },

        enabled_features    
            => sub { my $self = shift;
                     map { $_ => [ $self->modules_for_feature( $_ ) ] }
                        $self->list_enabled_features 
                   },
        features
            => sub { my $self = shift;
                     map { $_ => [ $self->modules_for_feature( $_ ) ] }
                        $self->list_features   
                   },
            ### make sure to do 'core' first, in case
            ### we are out of date ourselves
        all => [ qw|core dependencies enabled_features| ],
    };
    
    
    sub list_categories { return sort keys %$cache }

    sub list_modules_to_update {
        my $self = shift;
        my $cb   = $self->();
        my $conf = $cb->configure_object;
        my %hash = @_;
        
        my($type, $latest);
        my $tmpl = {
            update => { required => 1, store => \$type,
                         allow   => [ keys %$cache ], },
            latest => { default  => 0, store => \$latest, allow => BOOLEANS },                     
        };    
    
        {   local $Params::Check::ALLOW_UNKNOWN = 1;
            check( $tmpl, \%hash ) or return;
        }
    
        my $ref     = $cache->{$type};

        ### a list of ( feature1 => \@mods, feature2 => \@mods, etc )        
        my %list    = UNIVERSAL::isa( $ref, 'ARRAY' )
                            ? map { $cache->{$_}->( $self ) } @$ref
                            : $ref->( $self );

        ### filter based on whether we need the latest ones or not
        for my $aref ( values %list ) {              
              $aref = [ $latest 
                        ? grep { !$_->is_uptodate } @$aref
                        : grep { !$_->is_installed_version_sufficient } @$aref
                      ];
        }
        
        return %list;
    }
    
    sub selfupdate {
        my $self = shift;
        my $cb   = $self->();
        my $conf = $cb->configure_object;
        my %hash = @_;
    
        my $force;
        my $tmpl = {
            force  => { default => $conf->get_conf('force'), store => \$force },
        };    
    
        {   local $Params::Check::ALLOW_UNKNOWN = 1;
            check( $tmpl, \%hash ) or return;
        }
    
        my %list = $self->list_modules_to_update( %hash ) or return;

        ### just the modules please
        my @mods = map { @$_ } values %list;
        
        my $flag;
        for my $mod ( @mods ) {
            unless( $mod->install( force => $force ) ) {
                $flag++;
                error(loc("Failed to update module '%1'", $mod->name));
            }
        }
        
        return if $flag;
        return 1;
    }    

}

sub list_features {
    my $self = shift;
    return keys %{ $self->_get_config->{'features'} };
}

sub list_enabled_features {
    my $self = shift;
    my $cb   = $self->();
    
    my @enabled;
    for my $feat ( $self->list_features ) {
        my $ref = $self->_get_config->{'features'}->{$feat}->[1];
        push @enabled, $feat if $ref->($cb);
    }
    
    return @enabled;
}

sub modules_for_feature {
    my $self    = shift;
    my $feature = shift or return;
    my $as_hash = shift || 0;
    my $cb      = $self->();
    
    unless( exists $self->_get_config->{'features'}->{$feature} ) {
        error(loc("Unknown feature '%1'", $feature));
        return;
    }
    
    my $ref = $self->_get_config->{'features'}->{$feature}->[0];
    
    ### it's either a list of modules/versions or a subroutine that
    ### returns a list of modules/versions
    my $href = UNIVERSAL::isa( $ref, 'HASH' ) ? $ref : $ref->( $cb );
    
    return unless $href;    # nothing needed for the feature?

    return $href if $as_hash;
    return $self->_hashref_to_module( $href );
}


sub list_core_dependencies {
    my $self    = shift;
    my $as_hash = shift || 0;
    my $cb      = $self->();
    my $href    = $self->_get_config->{'dependencies'};

    return $href if $as_hash;
    return $self->_hashref_to_module( $href );
}

sub list_core_modules {
    my $self    = shift;
    my $as_hash = shift || 0;
    my $cb      = $self->();
    my $href    = $self->_get_config->{'core'};

    return $href if $as_hash;
    return $self->_hashref_to_module( $href );
}

sub _hashref_to_module {
    my $self = shift;
    my $cb   = $self->();
    my $href = shift or return;
    
    return map { 
            CPANPLUS::Selfupdate::Module->new(
                $cb->module_tree($_) => $href->{$_}
            )
        } keys %$href;
}        
    

{   package CPANPLUS::Selfupdate::Module;
    use base 'CPANPLUS::Module';
    
    ### stores module name -> cpanplus required version
    ### XXX only can deal with 1 pair!
    my %Cache = ();
    my $Acc   = 'version_required';
    
    sub new {
        my $class = shift;
        my $mod   = shift or return;
        my $ver   = shift;          return unless defined $ver;
        
        my $obj   = $mod->clone;    # clone the module object
        bless $obj, $class;         # rebless it to our class
        
        $obj->$Acc( $ver );
        
        return $obj;
    }

    sub version_required {
        my $self = shift;
        $Cache{ $self->name } = shift() if @_;
        return $Cache{ $self->name };
    }        

    
    sub is_installed_version_sufficient {
        my $self = shift;
        return $self->is_uptodate( version => $self->$Acc );
    }

}    

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
