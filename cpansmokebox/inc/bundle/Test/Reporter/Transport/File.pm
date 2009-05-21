use strict;
use warnings;
package Test::Reporter::Transport::File;
use base 'Test::Reporter::Transport';
use vars qw/$VERSION/;
$VERSION = '1.5203';
$VERSION = eval $VERSION;

sub new {
  my ($class, $dir) = @_;

  die "target directory '$dir' doesn't exist or can't be written to"
    unless -d $dir && -w $dir;

  return bless { dir => $dir } => $class;
}

sub send {
    my ($self, $report) = @_;
    $report->dir( $self->{dir} );
    return $report->write();
}

1;

__END__

