package App::Smoke::In::A::Box;

use strict;
use warnings;
use vars qw($VERSION);

$VERSION = '0.06';

'Smokebox';

__END__

=head1 NAME

App::Smoke::In::A::Box - CPAN Tester smoke environment boxed

=head1 SYNOPSIS

  $ /path/to/perl Makefile.PL 
  # answer the questions etc

  $ cd ~/cpansmokebox

  $ /path/to/perl bin/yactest-boxed

  # Smoking happens

=head1 DESCRIPTION

App::Smoke::In::A::Box is a standalone boxed CPAN Tester smoke environment. Built on L<CPANPLUS>
and L<CPANPLUS::YACSmoke> it enables an individual to simply download this distribution, extract it
somewhere, add their email address and optionally an email gateway to use and start CPAN Testing
against an arbitary C<perl> executable without worrying about the toolchain dependencies.

It provides L<CPANPLUS> and bundled modules required to bootstrap the CPAN Testing process.

Included are:

  CPANPLUS + its prereqs

  CPANPLUS::YACSmoke

  YAML::Tiny

  Test::Reporter

  Sort::Versions

  Regexp::Assemble

  Config::IniFiles

=head1 MAINTAINER

Chris C<BinGOs> Williams

=head1 KUDOS

Gabor Szabo, whose email to the cpan-testers-discuss mailing list inspired this utility.

Jos Boumans, who did all the real hard work with L<CPANPLUS> and cpanp-boxed.

Dave Golden, the L<Test::Reporter> guy.

Adam Kennedy, L<YAML::Tiny>

Ed Avis et al, L<Sort::Versions>

David Landgren, L<Regexp::Assemble>

=head1 LICENSE

Copyright E<copy> Chris Williams

This module may be used, modified, and distributed under the same terms as Perl itself. Please see the license that came with your Perl distribution for details.

=head1 SEE ALSO

L<CPANPLUS>

=cut
