package CPANPLUS::Module::Author;

use strict;

use CPANPLUS::Error;
use CPANPLUS::Internals::Constants;
use Params::Check               qw[check];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';

local $Params::Check::VERBOSE = 1;

my $tmpl = {
    author      => { required => 1 },   # full name of the author
    cpanid      => { required => 1 },   # cpan id
    email       => { default => '' },   # email address of the author
    _id         => { required => 1 },   # id of the Internals object that spawned us
};

### autogenerate accessors ###
for my $key ( keys %$tmpl ) {
    no strict 'refs';
    *{__PACKAGE__."::$key"} = sub {
        my $self = shift;
        $self->{$key} = $_[0] if @_;
        return $self->{$key};
    }
}

sub parent {
    my $self = shift;
    my $obj  = CPANPLUS::Internals->_retrieve_id( $self->_id );

    return $obj;
}

sub new {
    my $class   = shift;
    my %hash    = @_;

    ### don't check the template for sanity
    ### -- we know it's good and saves a lot of performance
    local $Params::Check::SANITY_CHECK_TEMPLATE = 0;

    my $object = check( $tmpl, \%hash ) or return;

    return bless $object, $class;
}

sub modules {
    my $self    = shift;
    my $cb      = $self->parent;

    my $aref = $cb->_search_module_tree(
                    type    => 'author',
                    ### XXX, depending on backend, this is either an object
                    ### or the cpanid string. Dont know an elegant way to
                    ### solve this right now, so passing both
                    allow   => [$self, $self->cpanid],
                );
    return @$aref if $aref;
    return;
}

sub distributions {
    my $self = shift;
    my %hash = @_;

    local $Params::Check::ALLOW_UNKNOWN = 1;
    local $Params::Check::NO_DUPLICATES = 1;

    my $mod;
    my $tmpl = {
        module  => { default => '', store => \$mod },
    };

    my $args = check( $tmpl, \%hash ) or return;

    ### if we didn't get a module object passed, we'll find one ourselves ###
    unless( $mod ) {
        my @list = $self->modules;
        if( @list ) {
            $mod = $list[0];
        } else {
            error( loc( "This author has released no modules" ) );
            return;
        }
    }

    my $file = $mod->checksums( %hash );
    my $href = $mod->_parse_checksums_file( file => $file ) or return;

    my @rv;
    for my $name ( keys %$href ) {

        ### shortcut asap, so we avoid extra ops. On big checksums files
        ### the call to clone() takes up a lot of time.
        ### .meta files are now also in the checksums file,
        ### which means we have to filter out things that dont
        ### match our regex
        next if $mod->package_extension( $name ) eq META_EXT;

        ### used to do this wiht ->clone. However, that calls ->dslip,
        ### (which is wrong anyway, as we're doing a different module),
        ### which in turn calls ->contains, which scans the entire
        ### module tree using _search_module_tree, which uses P::C
        ### and is therefor VERY VERY slow.
        ### so let's do this the direct way for speed ups.
        my $dist = CPANPLUS::Module::Fake->new(
                        module  =>  do { my $m = $mod->package_name( $name );
                                         $m =~ s/-/::/g; $m;
                                    },      
                        version =>  $mod->package_version(  $name ),
                        package =>  $name,
                        path    =>  $mod->path,     # same author after all
                        author  =>  $mod->author,   # same author after all
                        mtime   =>  $href->{$name}->{'mtime'},  # release date
                    );

        push @rv, $dist;
    }

    return @rv;
}


sub accessors { return keys %$tmpl };

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
