use strict;
BEGIN{ if (not $] < 5.006) { require warnings; warnings->import } }
package Test::Reporter::Transport;
use vars qw/$VERSION/;
$VERSION = '1.54';
$VERSION = eval $VERSION;

sub new     { die "Not implemented" }

sub send    { die "Not implemented" }

1;

__END__

