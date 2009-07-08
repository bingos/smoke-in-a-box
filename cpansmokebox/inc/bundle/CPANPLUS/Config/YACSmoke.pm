package CPANPLUS::Config::YACSmoke;

use strict;
use File::Spec;
use vars qw($VERSION);

$VERSION = '0.42';

sub setup {
  my $conf = shift;
  $conf->set_conf( base => File::Spec->catdir( $ENV{PERL5_YACSMOKE_BASE}, '.cpanplus' ) )
	if $ENV{PERL5_YACSMOKE_BASE};
  return 1;
}

'YACSmoking';

__END__

