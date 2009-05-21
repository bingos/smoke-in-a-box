package CPANPLUS::Shell::Default::Plugins::Remote;

use strict;

use Module::Load;
use Params::Check               qw[check];
use CPANPLUS::Error             qw[error msg];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';

### store the original prompt here, so we can restore it on disconnect
my $Saved_Prompt;

sub plugins { ( connect => 'connect', disconnect => 'disconnect' ) }

sub connect {
    my $class   = shift;
    my $shell   = shift;
    my $cb      = shift;
    my $cmd     = shift;
    my $input   = shift || '';
    my $opts    = shift || {};
    my $conf = $cb->configure_object;

    my $user; my $pass;
    {   local $Params::Check::ALLOW_UNKNOWN = 1;

        my $tmpl = {
            user => { default   => 'cpanpd',    store => \$user },
            pass => { required  => 1,           store => \$pass },
        };

         check( $tmpl, $opts ) or return;
    }

    my @parts = split /\s+/, $input;
    my $host = shift @parts || 'localhost';
    my $port = shift @parts || '1337';

    load IO::Socket;

    my $remote = IO::Socket::INET->new(
                        Proto       => "tcp",
                        PeerAddr    => $host,
                        PeerPort    => $port,
                    ) or (
                        error( loc( "Cannot connect to port '%1' ".
                                    "on host '%2'", $port, $host ) ),
                        return
                    );

    my $con = {
        connection  => $remote,
        username    => $user,
        password    => $pass,
    };

    ### store the connection
    $shell->remote( $con );

    my($status,$buffer) = $shell->__send_remote_command(
                            "VERSION=$CPANPLUS::Shell::Default::VERSION");

    if( $status ) {
        print "\n$buffer\n\n";

        print loc(  "Successfully connected to '%1' on port '%2'",
                    $host, $port );
        print "\n\n";
        print loc(  "Note that no output will appear until a command ".
                    "has completed\n-- this may take a while" );
        print "\n\n";

        ### save the original prompt
        $Saved_Prompt = $shell->prompt;

        $shell->prompt( $shell->brand .'@'. $host .':'. $port .'> ' );

    } else {
        print "\n$buffer\n\n";

        print loc(  "Failed to connect to '%1' on port '%2'",
                    $host, $port );
        print "\n\n";

        $shell->remote( undef );
    }
}

sub disconnect {
    my $class   = shift;
    my $shell   = shift;

    print "\n", ( $shell->remote
                    ? loc( "Disconnecting from remote host" )
                    : loc( "Not connected to remote host" )
            ), "\n\n";

    $shell->remote( undef );
    $shell->prompt( $Saved_Prompt );
}

sub connect_help {
    return loc( 
            "    /connect [HOST PORT]   # Connect to the remote machine,\n" .
            "                           # defaults taken from your config\n" .
            "        --user=USER        # Optional username\n" .
            "        --pass=PASS        # Optional password" );
}

sub disconnect_help {
    return loc(
            "    /disconnect            # Disconnect from the remote server" );
}

1; 
        
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

