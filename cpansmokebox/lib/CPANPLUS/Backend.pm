package CPANPLUS::Backend;

use strict;


use CPANPLUS::Error;
use CPANPLUS::Configure;
use CPANPLUS::Internals;
use CPANPLUS::Internals::Constants;
use CPANPLUS::Module;
use CPANPLUS::Module::Author;
use CPANPLUS::Backend::RV;

use FileHandle;
use File::Spec                  ();
use File::Spec::Unix            ();
use File::Basename              ();
use Params::Check               qw[check];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';

$Params::Check::VERBOSE = 1;

use vars qw[@ISA $VERSION];

@ISA     = qw[CPANPLUS::Internals];
$VERSION = $CPANPLUS::Internals::VERSION;

### mark that we're running under CPANPLUS to spawned processes
$ENV{'PERL5_CPANPLUS_IS_RUNNING'} = $$;

### XXX version.pm MAY format this version, if it's in use... :(
### so for consistency, just call ->VERSION ourselves as well.
$ENV{'PERL5_CPANPLUS_IS_VERSION'} = __PACKAGE__->VERSION;

sub new {
    my $class   = shift;
    my $conf;

    if( $_[0] && IS_CONFOBJ->( conf => $_[0] ) ) {
        $conf = shift;
    } else {
        $conf = CPANPLUS::Configure->new() or return;
    }

    my $self = $class->SUPER::_init( _conf => $conf );

    return $self;
}

sub module_tree {
    my $self    = shift;
    my $modtree = $self->_module_tree;

    if( @_ ) {
        my @rv;
        for my $name ( grep { defined } @_) {

            ### From John Malmberg: This is failing on VMS 
            ### because ODS-2 does not retain the case of 
            ### filenames that are created.
            ### The problem is the filename is being converted 
            ### to a module name and then looked up in the 
            ### %$modtree hash.
            ### 
            ### As a fix, we do a search on VMS instead --
            ### more cpu cycles, but it gets around the case
            ### problem --kane
            my ($modobj) = do {
                ON_VMS
                    ? $self->search(
                          type    => 'module',
                          allow   => [qr/^$name$/i],
                      )
                    : $modtree->{$name}
            };
            
            push @rv, $modobj || '';
        }
        return @rv == 1 ? $rv[0] : @rv;
    } else {
        return $modtree;
    }
}

sub author_tree {
    my $self        = shift;
    my $authtree    = $self->_author_tree;

    if( @_ ) {
        my @rv;
        for my $name (@_) {
            push @rv, $authtree->{$name} || '';
        }
        return @rv == 1 ? $rv[0] : @rv;
    } else {
        return $authtree;
    }
}

sub configure_object { return shift->_conf() };

sub selfupdate_object { return shift->_selfupdate() };

sub search {
    my $self = shift;
    my $conf = $self->configure_object;
    my %hash = @_;

    my ($type);
    my $args = do {
        local $Params::Check::NO_DUPLICATES = 0;
        local $Params::Check::ALLOW_UNKNOWN = 1;

        my $tmpl = {
            type    => { required => 1, allow => [CPANPLUS::Module->accessors(),
                            CPANPLUS::Module::Author->accessors()], store => \$type },
            allow   => { required => 1, default => [ ], strict_type => 1 },
        };

        check( $tmpl, \%hash )
    } or return;

    ### figure out whether it was an author or a module search
    ### when ambiguous, it'll be an author search.
    my $aref;
    if( grep { $type eq $_ } CPANPLUS::Module::Author->accessors() ) {
        $aref = $self->_search_author_tree( %$args );
    } else {
        $aref = $self->_search_module_tree( %$args );
    }

    return @$aref if $aref;
    return;
}

### XXX add direcotry_tree, packlist etc? or maybe remove files? ###
for my $func (qw[fetch extract install readme files distributions]) {
    no strict 'refs';

    *$func = sub {
        my $self = shift;
        my $conf = $self->configure_object;
        my %hash = @_;

        my ($mods);
        my $args = do {
            local $Params::Check::NO_DUPLICATES = 1;
            local $Params::Check::ALLOW_UNKNOWN = 1;

            my $tmpl = {
                modules     => { default  => [],    strict_type => 1,
                                 required => 1,     store => \$mods },
            };

            check( $tmpl, \%hash );
        } or return;

        ### make them all into module objects ###
        my %mods = map { $_ => $self->parse_module(module => $_) || '' } @$mods;

        my $flag; my $href;
        while( my($name,$obj) = each %mods ) {
            $href->{$name} = IS_MODOBJ->( mod => $obj )
                                ? $obj->$func( %$args )
                                : undef;

            $flag++ unless $href->{$name};
        }

        return CPANPLUS::Backend::RV->new(
                    function    => $func,
                    ok          => !$flag,
                    rv          => $href,
                    args        => \%hash,
                );
    }
}

sub parse_module {
    my $self = shift;
    my $conf = $self->configure_object;
    my %hash = @_;

    my $mod;
    my $tmpl = {
        module  => { required => 1, store => \$mod },
    };

    my $args = check( $tmpl, \%hash ) or return;

    return $mod if IS_MODOBJ->( module => $mod );

    ### ok, so it's not a module object, but a ref nonetheless?
    ### what are you smoking?
    if( ref $mod ) {
        error(loc("Can not parse module string from reference '%1'", $mod ));
        return;
    }
    
    ### check only for allowed characters in a module name
    unless( $mod =~ /[^\w:]/ ) {

        ### perhaps we can find it in the module tree?
        my $maybe = $self->module_tree($mod);
        return $maybe if IS_MODOBJ->( module => $maybe );
    }

    ### Special case arbitary file paths such as '.' etc.
    if (-d File::Spec->rel2abs($mod) ) {
        my $dir    = File::Spec->rel2abs($mod);
        my $parent = File::Spec->rel2abs( File::Spec->catdir( $dir, '..' ) );

        ### fix paths on VMS
        if (ON_VMS) {
            $dir    = VMS::Filespec::unixify($dir);
            $parent = VMS::Filespec::unixify($parent);
        }

        my $dist   = $mod = File::Basename::basename($dir);
        $dist     .= '-0'      unless $dist =~ /\-[0-9._]+$/;
        $dist     .= '.tar.gz' unless $dist =~ /\.[A-Za-z]+$/;

        my $modobj = CPANPLUS::Module::Fake->new(
                        module  => $mod,
                        version => 0,
                        package => $dist,
                        path    => $parent,
                        author  => CPANPLUS::Module::Author::Fake->new
                    );

        ### better guess for the version
        $modobj->version( $modobj->package_version ) 
            if defined $modobj->package_version;
        
        ### better guess at module name, if possible
        if ( my $pkgname = $modobj->package_name ) {
            $pkgname =~ s/-/::/g;
        
            ### no sense replacing it unless we changed something
            $modobj->module( $pkgname ) 
                if ($pkgname ne $modobj->package_name) || $pkgname !~ /-/;
        }                

        $modobj->status->fetch( $parent );
        $modobj->status->extract( $dir );
        $modobj->get_installer_type;
        return $modobj;
    }

    ### ok, so it looks like a distribution then?
    my @parts   = split '/', $mod;
    my $dist    = pop @parts;

    ### ah, it's a URL
    if( $mod =~ m|\w+://.+| ) {
        my $modobj = CPANPLUS::Module::Fake->new(
                        module  => $dist,
                        version => 0,
                        package => $dist,
                        path    => File::Spec::Unix->catdir(
                                        $conf->_get_mirror('base'),
                                        UNKNOWN_DL_LOCATION ),
                        author  => CPANPLUS::Module::Author::Fake->new
                    );
        
        ### set the fetch_from accessor so we know to by pass the
        ### usual mirrors
        $modobj->status->_fetch_from( $mod );
        
        ### better guess for the version
        $modobj->version( $modobj->package_version ) 
            if defined $modobj->package_version;
        
        ### better guess at module name, if possible
        if ( my $pkgname = $modobj->package_name ) {
            $pkgname =~ s/-/::/g;
        
            ### no sense replacing it unless we changed something
            $modobj->module( $pkgname ) 
                if ($pkgname ne $modobj->package_name) || $pkgname !~ /-/;
        }                
        
        return $modobj;      
    }
    
    ### perhaps we can find it's a third party module?
    {   my $modobj = CPANPLUS::Module::Fake->new(
                        module  => $mod,
                        version => 0,
                        package => $dist,
                        path    => File::Spec::Unix->catdir(
                                        $conf->_get_mirror('base'),
                                        UNKNOWN_DL_LOCATION ),
                        author  => CPANPLUS::Module::Author::Fake->new
                    );
        if( $modobj->is_third_party ) {
            my $info = $modobj->third_party_information;
            
            $modobj->author->author( $info->{author}     );
            $modobj->author->email(  $info->{author_url} );
            $modobj->description(    $info->{url} );

            return $modobj;
        }
    }

    unless( $dist ) {
        error( loc("%1 is not a proper distribution name!", $mod) );
        return;
    }
    
    ### there's wonky uris out there, like this:
    ### E/EY/EYCK/Net/Lite/Net-Lite-FTP-0.091
    ### compensate for that
    my $author;
    ### you probably have an A/AB/ABC/....../Dist.tgz type uri
    if( (defined $parts[0] and length $parts[0] == 1) and 
        (defined $parts[1] and length $parts[1] == 2) and
        $parts[2] =~ /^$parts[0]/i and $parts[2] =~ /^$parts[1]/i
    ) {   
        splice @parts, 0, 2;    # remove the first 2 entries from the list
        $author = shift @parts; # this is the actual author name then    

    ### we''ll assume a ABC/..../Dist.tgz
    } else {
        $author = shift @parts || '';
    }

    my($pkg, $version, $ext, $full) = 
        $self->_split_package_string( package => $dist );
    
    ### translate a distribution into a module name ###
    my $guess = $pkg; 
    $guess =~ s/-/::/g if $guess; 

    my $maybe = $self->module_tree( $guess );
    if( IS_MODOBJ->( module => $maybe ) ) {

        ### maybe you asked for a package instead
        if ( $maybe->package eq $mod ) {
            return $maybe;

        ### perhaps an outdated version instead?
        } elsif ( $version ) {
            my $auth_obj; my $path;

            ### did you give us an author part? ###
            if( $author ) {
                $auth_obj   = CPANPLUS::Module::Author::Fake->new(
                                    _id     => $maybe->_id,
                                    cpanid  => uc $author,
                                    author  => uc $author,
                                );
                $path       = File::Spec::Unix->catdir(
                                    $conf->_get_mirror('base'),
                                    substr(uc $author, 0, 1),
                                    substr(uc $author, 0, 2),
                                    uc $author,
                                    @parts,     #possible sub dirs
                                );
            } else {
                $auth_obj   = $maybe->author;
                $path       = $maybe->path;
            }        
        
            if( $maybe->package_name eq $pkg ) {
    
                my $modobj = CPANPLUS::Module::Fake->new(
                    module  => $maybe->module,
                    version => $version,
                    ### no extension? use the extension the original package
                    ### had instead
                    package => do { $ext 
                                        ? $full 
                                        : $full .'.'. $maybe->package_extension 
                                },
                    path    => $path,
                    author  => $auth_obj,
                    _id     => $maybe->_id
                );
                return $modobj;

            ### you asked for a specific version?
            ### assume our $maybe is the one you wanted,
            ### and fix up the version.. 
            } else {
    
                my $modobj = $maybe->clone;
                $modobj->version( $version );
                $modobj->package( 
                        $maybe->package_name .'-'. 
                        $version .'.'. 
                        $maybe->package_extension 
                );
                
                ### you wanted a specific author, but it's not the one
                ### from the module tree? we'll fix it up
                if( $author and $author ne $modobj->author->cpanid ) {
                    $modobj->author( $auth_obj );
                    $modobj->path( $path );
                }
                
                return $modobj;
            }
        
        ### you didn't care about a version, so just return the object then
        } elsif ( !$version ) {
            return $maybe;
        }

    ### ok, so we can't find it, and it's not an outdated dist either
    ### perhaps we can fake one based on the author name and so on
    } elsif ( $author and $version ) {

        ### be extra friendly and pad the .tar.gz suffix where needed
        ### it's just a guess of course, but most dists are .tar.gz
        $dist .= '.tar.gz' unless $dist =~ /\.[A-Za-z]+$/;

        ### XXX duplication from above for generating author obj + path...
        my $modobj = CPANPLUS::Module::Fake->new(
            module  => $guess,
            version => $version,
            package => $dist,
            author  => CPANPLUS::Module::Author::Fake->new(
                            author  => uc $author,
                            cpanid  => uc $author,
                            _id     => $self->_id,
                        ),
            path    => File::Spec::Unix->catdir(
                            $conf->_get_mirror('base'),
                            substr(uc $author, 0, 1),
                            substr(uc $author, 0, 2),
                            uc $author,
                            @parts,         #possible subdirs
                        ),
            _id     => $self->_id,
        );

        return $modobj;

    ### face it, we have /no/ idea what he or she wants...
    ### let's start putting the blame somewhere
    } else {

        unless( $author ) {
            error( loc( "'%1' does not contain an author part", $mod ) );
        }

        error( loc( "Cannot find '%1' in the module tree", $mod ) );
    }

    return;
}

sub reload_indices {
    my $self    = shift;
    my %hash    = @_;
    my $conf    = $self->configure_object;

    my $tmpl = {
        update_source   => { default    => 0, allow => [qr/^\d$/] },
        verbose         => { default    => $conf->get_conf('verbose') },
    };

    my $args = check( $tmpl, \%hash ) or return;

    ### make a call to the internal _module_tree, so it triggers cache
    ### file age
    my $uptodate = $self->_check_trees( %$args );


    return 1 if $self->_build_trees(
                                uptodate    => $uptodate,
                                use_stored  => 0,
                                verbose     => $conf->get_conf('verbose'),
                            );

    error( loc( "Error rebuilding source trees!" ) );

    return;
}

sub flush {
    my $self = shift;
    my $type = shift or return;

    my $cache = {
        methods => [ qw( methods load ) ],
        hosts   => [ qw( hosts ) ],
        modules => [ qw( modules lib) ],
        lib     => [ qw( lib ) ],
        load    => [ qw( load ) ],
        all     => [ qw( hosts lib modules methods load ) ],
    };

    my $aref = $cache->{$type}
                    or (
                        error( loc("No such cache '%1'", $type) ),
                        return
                    );

    return $self->_flush( list => $aref );
}

sub installed {
    my $self = shift;
    my $aref = $self->_all_installed;

    return @$aref if $aref;
    return;
}

sub local_mirror {
    my $self = shift;
    my $conf = $self->configure_object;
    my %hash = @_;

    my($path, $index, $force, $verbose);
    my $tmpl = {
        path        => { default => $conf->get_conf('base'),
                            store => \$path },
        index_files => { default => 1, store => \$index },
        force       => { default => $conf->get_conf('force'),
                            store => \$force },
        verbose     => { default => $conf->get_conf('verbose'),
                            store => \$verbose },
    };

    check( $tmpl, \%hash ) or return;

    unless( -d $path ) {
        $self->_mkdir( dir => $path )
                or( error( loc( "Could not create '%1', giving up", $path ) ),
                    return
                );
    } elsif ( ! -w _ ) {
        error( loc( "Could not write to '%1', giving up", $path ) );
        return;
    }

    my $flag;
    AUTHOR: {
    for my $auth (  sort { $a->cpanid cmp $b->cpanid }
                    values %{$self->author_tree}
    ) {

        MODULE: {
        my $i;
        for my $mod ( $auth->modules ) {
            my $fetchdir = File::Spec->catdir( $path, $mod->path );

            my %opts = (
                verbose     => $verbose,
                force       => $force,
                fetchdir    => $fetchdir,
            );

            ### only do this the for the first module ###
            unless( $i++ ) {
                $mod->_get_checksums_file(
                            %opts
                        ) or (
                            error( loc( "Could not fetch %1 file, " .
                                        "skipping author '%2'",
                                        CHECKSUMS, $auth->cpanid ) ),
                            $flag++, next AUTHOR
                        );
            }

            $mod->fetch( %opts )
                    or( error( loc( "Could not fetch '%1'", $mod->module ) ),
                        $flag++, next MODULE
                    );
        } }
    } }

    if( $index ) {
        for my $name (qw[auth dslip mod]) {
            $self->_update_source(
                        name    => $name,
                        verbose => $verbose,
                        path    => $path,
                    ) or ( $flag++, next );
        }
    }

    return !$flag;
}

sub autobundle {
    my $self = shift;
    my $conf = $self->configure_object;
    my %hash = @_;

    my($path,$force,$verbose);
    my $tmpl = {
        force   => { default => $conf->get_conf('force'), store => \$force },
        verbose => { default => $conf->get_conf('verbose'), store => \$verbose },
        path    => { default => File::Spec->catdir(
                                        $conf->get_conf('base'),
                                        $self->_perl_version( perl => $^X ),
                                        $conf->_get_build('distdir'),
                                        $conf->_get_build('autobundle') ),
                    store => \$path },
    };

    check($tmpl, \%hash) or return;

    unless( -d $path ) {
        $self->_mkdir( dir => $path )
                or( error(loc("Could not create directory '%1'", $path ) ),
                    return
                );
    }

    my $name; my $file;
    {   ### default filename for the bundle ###
        my($year,$month,$day) = (localtime)[5,4,3];
        $year += 1900; $month++;

        my $ext = 0;

        my $prefix  = $conf->_get_build('autobundle_prefix');
        my $format  = "${prefix}_%04d_%02d_%02d_%02d";

        BLOCK: {
            $name = sprintf( $format, $year, $month, $day, $ext);

            $file = File::Spec->catfile( $path, $name . '.pm' );

            -f $file ? ++$ext && redo BLOCK : last BLOCK;
        }
    }
    my $fh;
    unless( $fh = FileHandle->new( ">$file" ) ) {
        error( loc( "Could not open '%1' for writing: %2", $file, $! ) );
        return;
    }
    
    ### make sure we load the module tree *before* doing this, as it
    ### starts to chdir all over the place
    $self->module_tree;

    my $string = join "\n\n",
                    map {
                        join ' ',
                            $_->module,
                            ($_->installed_version(verbose => 0) || 'undef')
                    } sort {
                        $a->module cmp $b->module
                    }  $self->installed;

    my $now     = scalar localtime;
    my $head    = '=head1';
    my $pkg     = __PACKAGE__;
    my $version = $self->VERSION;
    my $perl_v  = join '', `$^X -V`;

    print $fh <<EOF;
package $name;

\$VERSION = '0.01';

1;

__END__

$head NAME

$name - Snapshot of your installation at $now

$head SYNOPSIS

To install the modules from this snapshot, run:

  cpanp -i file://full/path/to/${name}.pm

$head CONTENTS

$string

$head CONFIGURATION

$perl_v

$head AUTHOR

This bundle has been generated autotomatically by
    $pkg $version

EOF

    close $fh;

    return $file;
}

sub save_state {
    my $self = shift;
    return $self->_save_state( @_ );
}


### XXX these wrappers are not individually tested! only the underlying
### code through source.t and indirectly trought he CustomSource plugin.
=pod

sub list_custom_sources {
    return shift->__list_custom_module_sources( @_ );
}

sub add_custom_source {
    return shift->_add_custom_module_source( @_ );
}

### XXX do clever dispatching based on arg number?
sub remove_custom_source {
    return shift->_remove_custom_module_source( @_ );
}

sub update_custom_source {
    my $self = shift;
    
    ### if it mentions /remote/, the request is to update a single uri,
    ### not all the ones we have, so dispatch appropriately
    my $rv = grep( /remote/i, @_)
        ? $self->__update_custom_module_source( @_ )
        : $self->__update_custom_module_sources( @_ );

    return $rv;
}    

sub write_custom_source_index {
    return shift->__write_custom_module_index( @_ );
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

__END__

todo:
sub dist {          # not sure about this one -- probably already done
                      enough in Module.pm
sub reports {       # in Module.pm, wrapper here


