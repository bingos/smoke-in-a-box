package Module::Loaded;

use strict;
use Carp qw[carp];

BEGIN { use base 'Exporter';
        use vars qw[@EXPORT $VERSION];
        
        $VERSION = '0.02';
        @EXPORT  = qw[mark_as_loaded mark_as_unloaded is_loaded];
}

sub mark_as_loaded (*) {
    my $pm      = shift;
    my $file    = __PACKAGE__->_pm_to_file( $pm ) or return;
    my $who     = [caller]->[1];
    
    my $where   = is_loaded( $pm );
    if ( defined $where ) {
        carp "'$pm' already marked as loaded ('$where')";
    
    } else {
        $INC{$file} = $who;
    }
    
    return 1;
}

sub mark_as_unloaded (*) { 
    my $pm      = shift;
    my $file    = __PACKAGE__->_pm_to_file( $pm ) or return;

    unless( defined is_loaded( $pm ) ) {
        carp "'$pm' already marked as unloaded";

    } else {
        delete $INC{ $file };
    }
    
    return 1;
}

sub is_loaded (*) { 
    my $pm      = shift;
    my $file    = __PACKAGE__->_pm_to_file( $pm ) or return;

    return $INC{$file} if exists $INC{$file};
    
    return;
}


sub _pm_to_file {
    my $pkg = shift;
    my $pm  = shift or return;
    
    my $file = join '/', split '::', $pm;
    $file .= '.pm';
    
    return $file;
}    

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:

1;
