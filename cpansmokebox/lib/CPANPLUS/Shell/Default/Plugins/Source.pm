package CPANPLUS::Shell::Default::Plugins::Source;

use strict;
use CPANPLUS::Error             qw[error msg];
use Locale::Maketext::Simple    Class => 'CPANPLUS', Style => 'gettext';


sub plugins { return ( source => 'source' ) }

sub source {
    my $class   = shift;
    my $shell   = shift;
    my $cb      = shift;
    my $cmd     = shift;
    my $input   = shift || '';
    my $opts    = shift || {};
    my $verbose = $cb->configure_object->get_conf('verbose');
    
    for my $file ( split /\s+/, $input ) {
        my $fh = FileHandle->new("$file") or( 
            error(loc("Could not open file '%1': %2", $file, $!)),
            next
        );
        
        while( my $line = <$fh> ) {
            chomp $line;
            
            next if $line !~ /\S+/; # skip empty/whitespace only lines
            next if $line =~ /^#/;  # skip comments
            
            msg(loc("Dispatching '%1'", $line), $verbose); 
            return 1 if $shell->dispatch_on_input( input => $line );
        }
    }
}

sub source_help {
    return loc('    /source FILE [FILE ..] '.
               '# read in commands from the specified file' ),
}

1;

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

