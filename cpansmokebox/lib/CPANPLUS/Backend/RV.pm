package CPANPLUS::Backend::RV;

use strict;
use vars qw[$STRUCT];


use CPANPLUS::Error;
use CPANPLUS::Internals::Constants;

use IPC::Cmd                    qw[can_run run];
use Params::Check               qw[check];

use base 'Object::Accessor';

local $Params::Check::VERBOSE = 1;


sub new {
    my $class   = shift;
    my %hash    = @_;

    my $tmpl = {
        ok          => { required => 1, allow => BOOLEANS },
        args        => { required => 1 },
        rv          => { required => 1 },
        function    => { default => CALLING_FUNCTION->() },
    };

    my $args    = check( $tmpl, \%hash ) or return;
    my $self    = bless {}, $class;

#    $self->mk_accessors( qw[ok args function rv] );
    $self->mk_accessors( keys %$tmpl );

    ### set the values passed in the struct ###
    while( my($key,$val) = each %$args ) {
        $self->$key( $val );
    }

    return $self;
}

sub _ok { return shift->ok }
#sub _stringify  { Carp::carp( "stringifying!" ); overload::StrVal( shift ) }

### make it easier to check if($rv) { foo() }
### this allows people to not have to explicitly say
### if( $rv->ok ) { foo() }
### XXX add an explicit stringify, so it doesn't fall back to "bool"? :(
use overload bool       => \&_ok, 
#             '""'       => \&_stringify,
             fallback   => 1;

1;
