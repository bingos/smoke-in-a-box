package CPANPLUS::Module::Author::Fake;


use CPANPLUS::Module::Author;
use CPANPLUS::Internals;
use CPANPLUS::Error;

use strict;
use vars            qw[@ISA];
use Params::Check   qw[check];

@ISA = qw[CPANPLUS::Module::Author];

$Params::Check::VERBOSE = 1;

sub new {
    my $class = shift;
    my %hash  = @_;

    my $tmpl = {
        author  => { default => 'CPANPLUS Internals' },
        email   => { default => 'cpanplus-info@lists.sf.net' },
        cpanid  => { default => 'CPANPLUS' },
        _id     => { default => CPANPLUS::Internals->_last_id },
    };

    my $args = check( $tmpl, \%hash ) or return;

    my $obj = CPANPLUS::Module::Author->new( %$args ) or return;

    unless( $obj->_id ) {
        error(loc("No '%1' specified -- No CPANPLUS object associated!",'_id'));
        return;
    } 

    ### rebless object ###
    return bless $obj, $class;
}

1;


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
