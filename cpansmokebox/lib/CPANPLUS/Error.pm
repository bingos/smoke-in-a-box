package CPANPLUS::Error;

use strict;

use Log::Message private => 0;;

BEGIN {
    use Exporter;
    use Params::Check   qw[check];
    use vars            qw[@EXPORT @ISA $ERROR_FH $MSG_FH];

    @ISA        = 'Exporter';
    @EXPORT     = qw[cp_error cp_msg error msg];

    my $log     = new Log::Message;

    for my $func ( @EXPORT ) {
        no strict 'refs';
        
        my $prefix  = 'cp_';
        my $name    = $func;
        $name       =~ s/^$prefix//g;
        
        *$func = sub {
                        my $msg     = shift;
                        
                        ### no point storing non-messages
                        return unless defined $msg;
                        
                        $log->store(
                                message => $msg,
                                tag     => uc $name,
                                level   => $prefix . $name,
                                extra   => [@_]
                        );
                };
    }

    sub flush {
        return reverse $log->flush;
    }

    sub stack {
        return $log->retrieve( chrono => 1 );
    }

    sub stack_as_string {
        my $class = shift;
        my $trace = shift() ? 1 : 0;

        return join $/, map {
                        '[' . $_->tag . '] [' . $_->when . '] ' .
                        ($trace ? $_->message . ' ' . $_->longmess
                                : $_->message);
                    } __PACKAGE__->stack;
    }
}

package Log::Message::Handlers;
use Carp ();

{

    sub cp_msg {
        my $self    = shift;
        my $verbose = shift;

        ### so you don't want us to print the msg? ###
        return if defined $verbose && $verbose == 0;

        my $old_fh = select $CPANPLUS::Error::MSG_FH;

        print '['. $self->tag . '] ' . $self->message . "\n";
        select $old_fh;

        return;
    }

    sub cp_error {
        my $self    = shift;
        my $verbose = shift;

        ### so you don't want us to print the error? ###
        return if defined $verbose && $verbose == 0;

        my $old_fh = select $CPANPLUS::Error::ERROR_FH;

        ### is only going to be 1 for now anyway ###
        ### C::I may not be loaded, so do a can() check first
        my $cb      = CPANPLUS::Internals->can('_return_all_objects')
                        ? (CPANPLUS::Internals->_return_all_objects)[0]
                        : undef;

        ### maybe we didn't initialize an internals object (yet) ###
        my $debug   = $cb ? $cb->configure_object->get_conf('debug') : 0;
        my $msg     =  '['. $self->tag . '] ' . $self->message . "\n";

        ### i'm getting this warning in the test suite:
        ### Ambiguous call resolved as CORE::warn(), qualify as such or
        ### use & at CPANPLUS/Error.pm line 57.
        ### no idea where it's coming from, since there's no 'sub warn'
        ### anywhere to be found, but i'll mark it explicitly nonetheless
        ### --kane
        print $debug ? Carp::shortmess($msg) : $msg . "\n";

        select $old_fh;

        return;
    }
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
