package CPANPLUS;

use strict;
use Carp;

use CPANPLUS::Error;
use CPANPLUS::Backend;

use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';

BEGIN {
    use Exporter    ();
    use vars        qw( @EXPORT @ISA $VERSION );
    @EXPORT     =   qw( shell fetch get install );
    @ISA        =   qw( Exporter );
    $VERSION = "0.87_02";     #have to hardcode or cpan.org gets unhappy
}

### purely for backward compatibility, so we can call it from the commandline:
### perl -MCPANPLUS -e 'install Net::SMTP'
sub install {
    my $cpan = CPANPLUS::Backend->new;
    my $mod = shift or (
                    error(loc("No module specified!")), return
                );

    if ( ref $mod ) {
        error( loc( "You passed an object. Use %1 for OO style interaction",
                    'CPANPLUS::Backend' ));
        return;

    } else {
        my $obj = $cpan->module_tree($mod) or (
                        error(loc("No such module '%1'", $mod)),
                        return
                    );

        my $ok = $obj->install;

        $ok
            ? msg(loc("Installing of %1 successful", $mod),1)
            : msg(loc("Installing of %1 failed", $mod),1);

        return $ok;
    }
}

### simply downloads a module and stores it
sub fetch {
    my $cpan = CPANPLUS::Backend->new;

    my $mod = shift or (
                    error(loc("No module specified!")), return
                );

    if ( ref $mod ) {
        error( loc( "You passed an object. Use %1 for OO style interaction",
                    'CPANPLUS::Backend' ));
        return;

    } else {
        my $obj = $cpan->module_tree($mod) or (
                        error(loc("No such module '%1'", $mod)),
                        return
                    );

        my $ok = $obj->fetch( fetchdir => '.' );

        $ok
            ? msg(loc("Fetching of %1 successful", $mod),1)
            : msg(loc("Fetching of %1 failed", $mod),1);

        return $ok;
    }
}

### alias to fetch() due to compatibility with cpan.pm ###
sub get { fetch(@_) }


### purely for backwards compatibility, so we can call it from the commandline:
### perl -MCPANPLUS -e 'shell'
sub shell {
    my $option  = shift;

    ### since the user can specify the type of shell they wish to start
    ### when they call the shell() function, we have to eval the usage
    ### of CPANPLUS::Shell so we can set up all the checks properly
    eval { require CPANPLUS::Shell; CPANPLUS::Shell->import($option) };
    die $@ if $@;

    my $cpan = CPANPLUS::Shell->new();

    $cpan->shell();
}

1;

__END__

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
