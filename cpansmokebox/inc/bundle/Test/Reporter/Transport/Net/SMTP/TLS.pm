use strict;
BEGIN{ if (not $] < 5.006) { require warnings; warnings->import } }
package Test::Reporter::Transport::Net::SMTP::TLS;
use base 'Test::Reporter::Transport::Net::SMTP';
use vars qw/$VERSION/;
$VERSION = '1.54';
$VERSION = eval $VERSION;

use Net::SMTP::TLS;

1;

__END__

