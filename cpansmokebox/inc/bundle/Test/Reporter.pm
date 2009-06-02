# Test::Reporter - sends test results to cpan-testers@perl.org
#
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008 Adam J. Foxson.
# Copyright (C) 2008 David A. Golden
# Copyright (C) 2008 Ricardo Signes
# Copyright (C) 2004, 2005 Richard Soderberg.
# All rights reserved.
#
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

package Test::Reporter;
use 5.005;
use strict;
BEGIN{ if (not $] < 5.006) { require warnings; warnings->import } }
use Cwd;
use Config;
use Carp;
use Net::SMTP;
use FileHandle;
use File::Temp;
use Sys::Hostname;
use Time::Local ();
use vars qw($VERSION $AUTOLOAD $Tempfile $Report $DNS $Domain $Send);
use constant FAKE_NO_NET_DNS => 0;    # for debugging only
use constant FAKE_NO_NET_DOMAIN => 0; # for debugging only
use constant FAKE_NO_MAIL_SEND => 0;  # for debugging only

$VERSION = '1.54';

local $^W = 1;

sub new {
    my $type  = shift;
    my $class = ref($type) || $type;
    my $self  = {
        '_mx'                => ['mx.develooper.com'],
        '_address'           => 'cpan-testers@perl.org',
        '_grade'             => undef,
        '_distribution'      => undef,
        # XXX distfile => undef would break old clients :-( -- dagolden, 2009-03-30 
        '_distfile'          => '', 
        '_report'            => undef,
        '_subject'           => undef,
        '_from'              => undef,
        '_comments'          => '',
        '_errstr'            => '',
        '_via'               => '',
        '_timeout'           => 120,
        '_debug'             => 0,
        '_dir'               => '',
        '_subject_lock'      => 0,
        '_report_lock'       => 0,
        '_perl_version'      => {
            '_archname' => $Config{archname},
            '_osvers'   => $Config{osvers},
            '_myconfig' => Config::myconfig(),
        },
        '_transport'         => '',
        '_transport_args'    => [],
        '_mail_send_args'    => '', # deprecated -> use _transport_args
    };

    bless $self, $class;

    $self->{_attr} = {   
        map {$_ => 1} qw(   
            _address _distribution _distfile _comments _errstr _via _timeout _debug _dir
        )
    };

    warn __PACKAGE__, ": new\n" if $self->debug();
    croak __PACKAGE__, ": new: even number of named arguments required"
        unless scalar @_ % 2 == 0;

    $self->_process_params(@_) if @_;
    $self->transport('Net::SMTP') unless $self->transport();
    $self->_get_mx(@_) if $self->_have_net_dns();

    return $self;
}

sub _get_mx {
    my $self = shift;
    warn __PACKAGE__, ": _get_mx\n" if $self->debug();

    my %params = @_;

    return if exists $params{'mx'};

    my $dom = $params{'address'} || $self->address();
    my @mx;

    $dom =~ s/^.+\@//;

    for my $mx (sort {$a->preference() <=> $b->preference()} Net::DNS::mx($dom)) {
        push @mx, $mx->exchange();
    }

    if (not @mx) {
        warn __PACKAGE__,
            ": _get_mx: unable to find MX's for $dom, using defaults\n" if
                $self->debug();
        return;
    }

    $self->mx(\@mx);
}

sub _process_params {
    my $self = shift;
    warn __PACKAGE__, ": _process_params\n" if $self->debug();

    my %params   = @_;
    my @defaults = qw(
        mx address grade distribution distfile from comments via timeout debug dir perl_version transport_args transport );
    my %defaults = map {$_ => 1} @defaults;

    for my $param (keys %params) {   
        croak __PACKAGE__, ": new: parameter '$param' is invalid." unless
            exists $defaults{$param};
    }
    
    # XXX need to process transport_args directly rather than through 
    # the following -- store array ref directly
    for my $param (keys %params) {   
        $self->$param($params{$param});
    }
}

sub subject {
    my $self = shift;
    warn __PACKAGE__, ": subject\n" if $self->debug();
    croak __PACKAGE__, ": subject: grade and distribution must first be set"
        if not defined $self->{_grade} or not defined $self->{_distribution};

    return $self->{_subject} if $self->{_subject_lock};

    my $subject = uc($self->{_grade}) . ' ' . $self->{_distribution} .
        " $self->{_perl_version}->{_archname} $self->{_perl_version}->{_osvers}";

    return $self->{_subject} = $subject;
}

sub report {
    my $self = shift;
    warn __PACKAGE__, ": report\n" if $self->debug();

    return $self->{_report} if $self->{_report_lock};

    my $report;
    $report .= "This distribution has been tested as part of the CPAN Testers\n";
    $report .= "project, supporting the Perl programming language.  See\n";
    $report .= "http://wiki.cpantesters.org/ for more information or email\n"; 
    $report .= "questions to cpan-testers-discuss\@perl.org\n\n";

    if (not $self->{_comments}) {
        $report .= "\n\n--\n\n";
    }
    else {
        $report .= "\n--\n" . $self->{_comments} . "\n--\n\n";
    }

    $report .= $self->{_perl_version}->{_myconfig};

    chomp $report;
    chomp $report;

    return $self->{_report} = $report;
}

sub grade {
    my ($self, $grade) = @_;
    warn __PACKAGE__, ": grade\n" if $self->debug();

    my %grades    = (
        'pass'    => "all tests passed",
        'fail'    => "one or more tests failed",
        'na'      => "distribution will not work on this platform",
        'unknown' => "distribution did not include tests",
    );

    return $self->{_grade} if scalar @_ == 1;

    croak __PACKAGE__, ":grade: '$grade' is invalid, choose from: " .
        join ' ', keys %grades unless $grades{$grade};

    return $self->{_grade} = $grade;
}

sub transport {
    my $self = shift;
    warn __PACKAGE__, ": transport\n" if $self->debug();

    return $self->{_transport} unless scalar @_;

    my $transport = shift;

    my $transport_class = "Test::Reporter::Transport::$transport";
    unless ( eval "require $transport_class; 1" ) { 
        croak __PACKAGE__ . ": could not load '$transport_class'\n$@\n";
    }

    my @args = @_;

    if ( @args && $transport eq 'Mail::Send' && ref $args[0] eq 'ARRAY' ) {
        # treat as old form of Mail::Send arguments and convert to list
        $self->transport_args(@{$args[0]});
    }
    elsif ( @args ) {
        $self->transport_args(@args);
    }

    return $self->{_transport} = $transport;
}

sub edit_comments {
    my($self, %args) = @_;
    warn __PACKAGE__, ": edit_comments\n" if $self->debug();

    my %tempfile_args = (
        UNLINK => 1,
        SUFFIX => '.txt',
        EXLOCK => 0,
    );

    if (exists $args{'suffix'} && defined $args{'suffix'} && length $args{'suffix'}) {
        $tempfile_args{SUFFIX} = $args{'suffix'};
        # prefix the extension with a period, if the user didn't.
        $tempfile_args{SUFFIX} =~ s/^(?!\.)(?=.)/./;
    }

    ($Tempfile, $Report) = File::Temp::tempfile(%tempfile_args);

    print $Tempfile $self->{_comments};

    $self->_start_editor();

    my $comments;
    {
        local $/;
        open FH, $Report or die __PACKAGE__, ": Can't open comment file '$Report': $!";
        $comments = <FH>;
        close FH or die __PACKAGE__, ": Can't close comment file '$Report': $!";
    }

    chomp $comments;

    $self->{_comments} = $comments;

    return;
}

sub send {
    my ($self) = @_;
    warn __PACKAGE__, ": send\n" if $self->debug();

    $self->from();
    $self->report();
    $self->subject();

    return unless $self->_verify();

    if ($self->_is_a_perl_release($self->distribution())) {
        $self->errstr(__PACKAGE__ . ": use perlbug for reporting test " .
            "results against perl itself");
        return;
    }

    my $transport_type  = $self->transport() || 'Net::SMTP';
    my $transport_class = "Test::Reporter::Transport::$transport_type";
    my $transport = $transport_class->new( $self->transport_args() );

    unless ( eval { $transport->send( $self ) } ) {
        $self->errstr(__PACKAGE__ . ": error from '$transport_class:'\n$@\n");
        return;
    }

    return 1;
}

sub write {
    my $self = shift;
    warn __PACKAGE__, ": write\n" if $self->debug();

    my $from = $self->from();
    my $report = $self->report();
    my $subject = $self->subject();
    my $distribution = $self->distribution();
    my $grade = $self->grade();
    my $dir = $self->dir() || cwd;

    return unless $self->_verify();

    $distribution =~ s/[^A-Za-z0-9\.\-]+//g;

    my($fh, $file); unless ($fh = $_[0]) {
        $file = "$grade.$distribution.$self->{_perl_version}->{_archname}.$self->{_perl_version}->{_osvers}.${\(time)}.$$.rpt";

        if ($^O eq 'VMS') {
            $file = "$grade.$distribution.$self->{_perl_version}->{_archname}";
            my $ext = "$self->{_perl_version}->{_osvers}.${\(time)}.$$.rpt";
            # only 1 period in filename
            # we also only have 39.39 for filename
            $file =~ s/\./_/g;
            $ext  =~ s/\./_/g;
            $file = $file . '.' . $ext;
        }

        $file = File::Spec->catfile($dir, $file);

        warn $file if $self->debug();
        $fh = FileHandle->new();
        open $fh, ">$file" or die __PACKAGE__, ": Can't open report file '$file': $!";
    }
    print $fh "From: $from\n";
    print $fh "Subject: $subject\n";
    print $fh "Report: $report";
    unless ($_[0]) {
        close $fh or die __PACKAGE__, ": Can't close report file '$file': $!";
        warn $file if $self->debug();
        return $file;
    } else {
        return $fh;
    }
}

sub read {
    my ($self, $file) = @_;
    warn __PACKAGE__, ": read\n" if $self->debug();

    my $buffer;

    {
        local $/;
        open REPORT, $file or die __PACKAGE__, ": Can't open report file '$file': $!";
        $buffer = <REPORT>;
        close REPORT or die __PACKAGE__, ": Can't close report file '$file': $!";
    }

    if (my ($from, $subject, $report) = $buffer =~ /^From:\s(.+)Subject:\s(.+)Report:\s(.+)$/s) {
        my ($grade, $distribution) = (split /\s/, $subject)[0,1];
        chomp($from);
        chomp($subject);
        $self->{_from} = $from;
        $self->{_subject} = $subject;
        $self->{_report} = $report;
        $self->{_grade} = lc $grade;
        $self->{_distribution} = $distribution;
        $self->{_subject_lock} = 1;
        $self->{_report_lock} = 1;
    } else {
        die __PACKAGE__, ": Failed to parse report file '$file'\n";
    }

    return $self;
}

sub _verify {
    my $self = shift;
    warn __PACKAGE__, ": _verify\n" if $self->debug();

    my @undefined;

    for my $key (keys %{$self}) {
        push @undefined, $key unless defined $self->{$key};
    }

    $self->errstr(__PACKAGE__ . ": Missing values for: " .
        join ', ', map {$_ =~ /^_(.+)$/} @undefined) if
        scalar @undefined > 0;
    return $self->errstr() ? return 0 : return 1;
}

# Courtesy of Email::MessageID
sub message_id {
    my $self = shift;
    warn __PACKAGE__, ": message_id\n" if $self->debug();

    my $unique_value = 0;
    my @CHARS = ('A'..'F','a'..'f',0..9);
    my $length = 3;

    $length = rand(8) until $length > 3;

    my $pseudo_random = join '', (map $CHARS[rand $#CHARS], 0 .. $length), $unique_value++;
    my $user = join '.', time, $pseudo_random, $$;

    return '<' . $user . '@' . Sys::Hostname::hostname() . '>';
}

sub from {
    my $self = shift;
    warn __PACKAGE__, ": from\n" if $self->debug();

    if (@_) {
        $self->{_from} = shift;
        return $self->{_from};
    }
    else {
        return $self->{_from} if defined $self->{_from} and $self->{_from};
        $self->{_from} = $self->_mailaddress();
        return $self->{_from};
    }

}

sub mx {
    my $self = shift;
    warn __PACKAGE__, ": mx\n" if $self->debug();

    if (@_) {
        my $mx = shift;
        croak __PACKAGE__,
            ": mx: array reference required" if ref $mx ne 'ARRAY';
        $self->{_mx} = $mx;
    }

    return $self->{_mx};
}

# Deprecated, but kept for backwards compatibility
# Passes through to transport_args -- converting from array ref to list to
# store and converting from list to array ref to get
sub mail_send_args {
    my $self = shift;
    warn __PACKAGE__, ": mail_send_args\n" if $self->debug();
    croak __PACKAGE__, ": mail_send_args cannot be called unless Mail::Send is installed\n" unless $self->_have_mail_send();
    if (@_) {
        my $mail_send_args = shift;
        croak __PACKAGE__, ": mail_send_args: array reference required\n" 
            if ref $mail_send_args ne 'ARRAY';
        $self->transport_args(@$mail_send_args);
    }
    return [ $self->transport_args() ];
}



sub transport_args {
    my $self = shift;
    warn __PACKAGE__, ": transport_args\n" if $self->debug();
    
    if (@_) {
        $self->{_transport_args} = ref $_[0] eq 'ARRAY' ? $_[0] : [ @_ ];
    }

    return @{ $self->{_transport_args} };
}


sub perl_version  {
    my $self = shift;
    warn __PACKAGE__, ": perl_version\n" if $self->debug();

    if( @_) {
        my $perl = shift;
        my $q = ( ($^O eq "MSWin32") || ($^O eq 'VMS') ) ? '"' : "'"; # quote for command-line perl
        my $magick = int(rand(1000));                                 # just to check that we get a valid result back
        my $cmd  = "$perl -MConfig -e$q print qq{$magick\n\$Config{archname}\n\$Config{osvers}\n},Config::myconfig();$q";
        if($^O eq 'VMS'){
            my $sh = $Config{'sh'};
            $cmd  = "$sh $perl $q-MConfig$q -e$q print qq{$magick\\n\$Config{archname}\\n\$Config{osvers}\\n},Config::myconfig();$q";
        }
        my $conf = `$cmd`;
        my %conf;
        ( @conf{ qw( magick _archname _osvers _myconfig) } ) = split( /\n/, $conf, 4);
        croak __PACKAGE__, ": cannot get perl version info from $perl: $conf" if( $conf{magick} ne $magick);
        delete $conf{magick};
        $self->{_perl_version} = \%conf;
   }
   return $self->{_perl_version};
}

sub AUTOLOAD {
    my $self               = $_[0];
    my ($package, $method) = ($AUTOLOAD =~ /(.*)::(.*)/);

    return if $method =~ /^DESTROY$/;

    unless ($self->{_attr}->{"_$method"}) {
        croak __PACKAGE__, ": No such method: $method; aborting";
    }

    my $code = q{
        sub {   
            my $self = shift;
            warn __PACKAGE__, ": METHOD\n" if $self->{_debug};
            $self->{_METHOD} = shift if @_;
            return $self->{_METHOD};
        }
    };

    $code =~ s/METHOD/$method/g;

    {
        no strict 'refs';
        *$AUTOLOAD = eval $code;
    }

    goto &$AUTOLOAD;
}

sub _have_net_dns {
    my $self = shift;
    warn __PACKAGE__, ": _have_net_dns\n" if $self->debug();

    return $DNS if defined $DNS;
    return 0 if FAKE_NO_NET_DNS;

    $DNS = eval {require Net::DNS};
}

sub _have_net_domain {
    my $self = shift;
    warn __PACKAGE__, ": _have_net_domain\n" if $self->debug();

    return $Domain if defined $Domain;
    return 0 if FAKE_NO_NET_DOMAIN;

    $Domain = eval {require Net::Domain};
}

sub _have_mail_send {
    my $self = shift;
    warn __PACKAGE__, ": _have_mail_send\n" if $self->debug();

    return $Send if defined $Send;
    return 0 if FAKE_NO_MAIL_SEND;

    $Send = eval {require Mail::Send};
}

sub _start_editor {
    my $self = shift;
    warn __PACKAGE__, ": _start_editor\n" if $self->debug();

    my $editor = $ENV{VISUAL} || $ENV{EDITOR} || $ENV{EDIT}
        || ($^O eq 'VMS'     and "edit/tpu")
        || ($^O eq 'MSWin32' and "notepad")
        || 'vi';

    $editor = $self->_prompt('Editor', $editor);

    die __PACKAGE__, ": The editor `$editor' could not be run on '$Report': $!" if system "$editor $Report";
    die __PACKAGE__, ": Report has disappeared; terminated" unless -e $Report;
    die __PACKAGE__, ": Empty report; terminated" unless -s $Report > 2;
}

sub _prompt {
    my $self = shift;
    warn __PACKAGE__, ": _prompt\n" if $self->debug();

    my ($label, $default) = @_;

    printf "$label%s", (" [$default]: ");
    my $input = scalar <STDIN>;
    chomp $input;

    return (length $input) ? $input : $default;
}

# From Mail::Util 1.74 (c) 1995-2001 Graham Barr (c) 2002-2005 Mark Overmeer
{   
  # cache the mail domain, so we don't try to resolve this *every* time
  # (thanks you kane)
  my $domain;     

  sub _maildomain {
      my $self = shift;
      warn __PACKAGE__, ": _maildomain\n" if $self->debug();
      
      # use cached value if set
      return $domain if defined $domain;

      # prefer MAILDOMAIN if set
      if ( defined $ENV{MAILDOMAIN} ) {
        return $domain = $ENV{MAILDOMAIN};
      }

      local *CF;
      local $_;

      my @sendmailcf = qw(
          /etc /etc/sendmail /etc/ucblib /etc/mail /usr/lib /var/adm/sendmail
      );

      my $config = (grep(-r, map("$_/sendmail.cf", @sendmailcf)))[0];

      if (defined $config && open(CF, $config)) {
          my %var;
          while (<CF>) {
              if (my ($v, $arg) = /^D([a-zA-Z])([\w.\$\-]+)/) {
                  $arg =~ s/\$([a-zA-Z])/exists $var{$1} ? $var{$1} : '$'.$1/eg;
                  $var{$v} = $arg;
              }
          }
          close(CF) || die $!;
          $domain = $var{j} if defined $var{j};
          $domain = $var{M} if defined $var{M};

          $domain = $1
              if ($domain && $domain =~ m/([A-Za-z0-9](?:[\.\-A-Za-z0-9]+))/);

          undef $domain if $^O eq 'darwin' && $domain =~ /\.local$/;

          return $domain if (defined $domain && $domain !~ /\$/);
      }

      if (open(CF, "/usr/lib/smail/config")) {
          while (<CF>) {
              if (/\A\s*hostnames?\s*=\s*(\S+)/) {
                  $domain = (split(/:/,$1))[0];
                  undef $domain if $^O eq 'darwin' && $domain =~ /\.local$/;
                  last if defined $domain and $domain;
              }
          }
          close(CF) || die $!;

          return $domain if defined $domain;
      }

      if (eval {require Net::SMTP}) {
          my $host;

              for $host (qw(mailhost smtp localhost)) {
              
                  # default timeout is 120, which is Very Very Long, so lower
                  # it to 5 seconds. Total slowdown will not be more than
                  # 15 seconds ( 5 x @hosts ) --kane
                  my $smtp = eval {Net::SMTP->new($host, Timeout => 5)};

              if (defined $smtp) {
                  $domain = $smtp->domain;
                  $smtp->quit;
                  undef $domain if $^O eq 'darwin' && $domain =~ /\.local$/;
                  last if defined $domain and $domain;
              }
          }
      }

      unless (defined $domain) {
          if ($self->_have_net_domain()) {
              ###################################################################
              # The below statement might possibly exhibit intermittent blocking
              # behavior. Be advised!
              ###################################################################
              $domain = Net::Domain::domainname();
              undef $domain if $^O eq 'darwin' && $domain =~ /\.local$/;
          }
      }

      $domain = "localhost" unless defined $domain;

      return $domain;
  }
}

# From Mail::Util 1.74 (c) 1995-2001 Graham Barr (c) 2002-2005 Mark Overmeer
sub _mailaddress {
    my $self = shift;
    warn __PACKAGE__, ": _mailaddress\n" if $self->debug();

    my $mailaddress = $ENV{MAILADDRESS};
    $mailaddress ||= $ENV{USER}    ||
                     $ENV{LOGNAME} ||
                     eval {getpwuid($>)} ||
                     "postmaster";
    $mailaddress .= '@' . $self->_maildomain() unless $mailaddress =~ /\@/;
    $mailaddress =~ s/(^.*<|>.*$)//g;

    my $realname = $self->_realname();
    if ($realname) {
        $mailaddress = "$mailaddress ($realname)";
    }

    return $mailaddress;
}

sub _realname {
    my $self = shift;
    warn __PACKAGE__, ": _realname\n" if $self->debug();

    my $realname = '';

    $realname =
        eval {(split /,/, (getpwuid($>))[6])[0]} ||
        $ENV{QMAILNAME}                          ||
        $ENV{REALNAME}                           ||
        $ENV{USER};

    return $realname;
}

sub _is_a_perl_release {
    my $self = shift;
    warn __PACKAGE__, ": _is_a_perl_release\n" if $self->debug();

    my $perl = shift;

    return $perl =~ /^perl-?\d\.\d/;
}

# need a true value
1;

__END__

1;
