package CPANPLUS::Dist::Base;

use strict;

use base    qw[CPANPLUS::Dist];
use vars    qw[$VERSION];
$VERSION =  $CPANPLUS::Internals::VERSION = $CPANPLUS::Internals::VERSION;


sub methods { 
    return qw[format_available init prepare create install uninstall] 
}

sub format_available { return 1 }


sub init { return 1; }

sub prepare { 
    ### just in case you already did a create call for this module object
    ### just via a different dist object
    my $dist        = shift;
    my $self        = $dist->parent;
    my $dist_cpan   = $self->status->dist_cpan;

    my $cb   = $self->parent;
    my $conf = $cb->configure_object;

    $dist->status->prepared( $dist_cpan->prepare( @_ ) );
}

sub create { 
    ### just in case you already did a create call for this module object
    ### just via a different dist object
    my $dist        = shift;
    my $self        = $dist->parent;
    my $dist_cpan   = $self->status->dist_cpan;
    $dist           = $self->status->dist   if      $self->status->dist;
    $self->status->dist( $dist )            unless  $self->status->dist;

    my $cb      = $self->parent;
    my $conf    = $cb->configure_object;
    my $format  = ref $dist;

    ### make sure to set this variable, if the caller hasn't yet
    ### just so we have some clue where the dist left off.
    $dist->status->dist( $dist_cpan->status->distdir )
        unless defined $dist->status->dist;

    $dist->status->created( $dist_cpan->create(prereq_format => $format, @_) );
}

sub install { 
    ### just in case you already did a create call for this module object
    ### just via a different dist object
    my $dist        = shift;
    my $self        = $dist->parent;
    my $dist_cpan   = $self->status->dist_cpan;    

    my $cb   = $self->parent;
    my $conf = $cb->configure_object;

    $dist->status->installed( $dist_cpan->install( @_ ) );
}

sub uninstall { 
    ### just in case you already did a create call for this module object
    ### just via a different dist object
    my $dist        = shift;
    my $self        = $dist->parent;
    my $dist_cpan   = $self->status->dist_cpan;    

    my $cb   = $self->parent;
    my $conf = $cb->configure_object;

    $dist->status->uninstalled( $dist_cpan->uninstall( @_ ) );
}

1;              

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
