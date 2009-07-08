package Config::IniFiles;

use vars qw($VERSION);

$VERSION = "2.52";

require 5.004;
use strict;
use Carp;
use Symbol 'gensym','qualify_to_ref';   # For the 'any data type' hack

@Config::IniFiles::errors = ( );

#	$Header: /home/shlomi/progs/perl/cpan/Config/IniFiles/config-inifiles-cvsbackup/config-inifiles/IniFiles.pm,v 2.41 2003-12-08 10:50:56 domq Exp $

sub new {
  my $class = shift;
  my %parms = @_;

  my $errs = 0;
  my @groups = ( );

  my $self = bless {
	default => '',
	imported =>undef,
	v =>{},
	cf => undef,
	firstload => 1,
  }, $class;

  if( ref($parms{-import}) && ($parms{-import}->isa('Config::IniFiles')) ) {
    $self->{imported}=$parms{-import}; # ReadConfig will load the data
    $self->{negativedeltas}=1;
  } elsif( defined $parms{-import} ) {
    carp "Invalid -import value \"$parms{-import}\" was ignored.";
  } # end if
  delete $parms{-import};

  # Copy the original parameters so we 
  # can use them when we build new sections 
  %{$self->{startup_settings}} = %parms;

  # Parse options
  my($k, $v);
  local $_;
  $self->{nocase} = 0;

  # Handle known parameters first in this order, 
  # because each() could return parameters in any order
  if (defined ($v = delete $parms{'-file'})) {
    # Should we be pedantic and check that the file exists?
    # .. no, because now it could be a handle, IO:: object or something else
    $self->{cf} = $v;
  }
  if (defined ($v = delete $parms{'-nocase'})) {
    $self->{nocase} = $v ? 1 : 0;
  }  
  if (defined ($v = delete $parms{'-default'})) {
    $self->{default} = $self->{nocase} ? lc($v) : $v;
  }
  if (defined ($v = delete $parms{'-reloadwarn'})) {
    $self->{reloadwarn} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-allowcontinue'})) {
    $self->{allowcontinue} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-allowempty'})) {
	  $self->{allowempty} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-negativedeltas'})) {
	  $self->{negativedeltas} = $v ? 1 : 0;
  }
  if (defined ($v = delete $parms{'-commentchar'})) {
    if(!defined $v || length($v) != 1) {
      carp "Comment character must be unique.";
      $errs++;
    }
    elsif($v =~ /[\[\]=\w]/) {
      # must not be square bracket, equal sign or alphanumeric
      carp "Illegal comment character.";
      $errs++;
    } 
    else {
      $self->{comment_char} = $v;
    }
  }
  if (defined ($v = delete $parms{'-allowedcommentchars'})) {
    # must not be square bracket, equal sign or alphanumeric
    if(!defined $v || $v =~ /[\[\]=\w]/) {
      carp "Illegal value for -allowedcommentchars.";
      $errs++;
    }
    else {
      $self->{allowed_comment_char} = $v;
    }
  }
  $self->{comment_char} = '#' unless exists $self->{comment_char};
  $self->{allowed_comment_char} = ';' unless exists $self->{allowed_comment_char};
  # make sure that comment character is always allowed
  $self->{allowed_comment_char} .= $self->{comment_char};

  $self->{_comments_at_end_of_file} = [];

  # Any other parameters are unkown
  while (($k, $v) = each %parms) {
    carp "Unknown named parameter $k=>$v";
    $errs++;
  }

  return undef if $errs;
  
  if ($self->ReadConfig) {
    return $self;
  } else {
    return undef;
  }
}


sub val {
  my ($self, $sect, $parm, $def) = @_;

  # Always return undef on bad parameters
  return if not defined $sect;
  return if not defined $parm;
  
  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }
  
  my $val = defined($self->{v}{$sect}{$parm}) ?
    $self->{v}{$sect}{$parm} :
    $self->{v}{$self->{default}}{$parm};
  
  # If the value is undef, make it $def instead (which could just be undef)
  $val = $def unless defined $val;
  
  # Return the value in the desired context
  if (wantarray) {
    if (ref($val) eq "ARRAY") {
      return @$val;
    } elsif (defined($val)) {
      return $val;
    } else {
      return;
    }
  } elsif (ref($val) eq "ARRAY") {
  	if (defined ($/)) {
	    return join "$/", @$val;
	} else {
		return join "\n", @$val;
	}
  } else {
    return $val;
  }
}



sub exists {
	my ($self, $sect, $parm)=@_;

    if ($self->{nocase}) {
        $sect = lc($sect);
        $parm = lc($parm);
    }
    
	return (exists $self->{v}{$sect}{$parm});
}



sub push {
  my ($self, $sect, $parm, @vals)=@_;

  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  return undef if (! defined($self->{v}{$sect}{$parm}));

  return 1 if (! @vals);

  $self->_touch_parameter($sect, $parm);

  $self->{EOT}{$sect}{$parm} = 'EOT' if
	(!defined $self->{EOT}{$sect}{$parm});

  $self->{v}{$sect}{$parm} = [$self->{v}{$sect}{$parm}] unless
     (ref($self->{v}{$sect}{$parm}) eq "ARRAY");

  CORE::push @{$self->{v}{$sect}{$parm}}, @vals;
  return 1;
}

sub setval {
  my $self = shift;
  my $sect = shift;
  my $parm = shift;
  my @val  = @_;

  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  if (defined($self->{v}{$sect}{$parm})) {
	$self->_touch_parameter($sect, $parm);
    if (@val > 1) {
      $self->{v}{$sect}{$parm} = \@val;
	  $self->{EOT}{$sect}{$parm} = 'EOT';
    } else {
      $self->{v}{$sect}{$parm} = shift @val;
    }
    return 1;
  } else {
    return undef;
  }
}

sub newval {
  my $self = shift;
  my $sect = shift;
  my $parm = shift;
  my @val  = @_;
  
  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  $self->AddSection($sect);

  CORE::push(@{$self->{parms}{$sect}}, $parm) 
      unless (grep {/^\Q$parm\E$/} @{$self->{parms}{$sect}} );

  $self->_touch_parameter($sect, $parm);
  if (@val > 1) {
    $self->{v}{$sect}{$parm} = \@val;
    $self->{EOT}{$sect}{$parm} = 'EOT' unless defined $self->{EOT}{$sect}{$parm};
  } else {
    $self->{v}{$sect}{$parm} = shift @val;
  }
  return 1
}

sub delval {
  my $self = shift;
  my $sect = shift;
  my $parm = shift;
  
  return undef if not defined $sect;
  return undef if not defined $parm;

  if ($self->{nocase}) {
    $sect = lc($sect);
    $parm = lc($parm);
  }

  @{$self->{parms}{$sect}} = grep !/^\Q$parm\E$/, @{$self->{parms}{$sect}};
  $self->_touch_parameter($sect, $parm);
  delete $self->{v}{$sect}{$parm};
  return 1
}

# Auxillary function to make deep (aliasing-free) copies of data
# structures.  Ignores blessed objects in tree (could be taught not
# to, if needed)
sub _deepcopy {
  my $ref=shift;

  if (! ref($ref)) { return $ref; }

  local $_;

  if (UNIVERSAL::isa($ref, "ARRAY")) {
          return [map {_deepcopy($_)} @$ref];
  }

  if (UNIVERSAL::isa($ref, "HASH")) {
          my $return={};
          foreach my $k (keys %$ref) {
                  $return->{$k}=_deepcopy($ref->{$k});
          }
          return $return;
  }

  carp "Unhandled data structure in $ref, cannot _deepcopy()";
}

# Internal method, gets the next line, taking proper care of line endings.
sub _nextline {
	my ($self, $fh)=@_;
	local $_;
	if (!exists $self->{line_ends}) {
		# no $self->{line_ends} is a hint set by caller that we are at
		# the first line (kludge kludge).
		do {
			local $/=\1; my $nextchar=<$fh>;
			return undef if (!defined $nextchar);
			$_ .= $nextchar;
		} until (m/(\015\012?|\012|\025|\n)$/s);
		$self->{line_ends}=$1;

		# If there's a UTF BOM (Byte-Order-Mark) in the first
		# character of the first line then remove it before processing
		# (http://www.unicode.org/unicode/faq/utf_bom.html#22)
		s/^﻿//;

		return $_;
	} else {
		local $/=$self->{line_ends};
		return scalar <$fh>;
	}
}

# Internal method, closes or resets the file handle. To be called
# whenever ReadConfig() returns.
sub _rollback {
	my ($self, $fh)=@_;
  # Only close if this is a filename, if it's
  # an open handle, then just roll back to the start
  if( !ref($self->{cf}) ) {
    close($fh);
  } else {
    # Attempt to rollback to beginning, no problem if this fails (e.g. STDIN)
    seek( $fh, 0, 0 );
  } # end if
}


sub ReadConfig {
  my $self = shift;

  my($lineno, $sect);
  my($group, $groupmem);
  my($parm, $val);
  my @cmts;

  @Config::IniFiles::errors = ( );

  # Initialize (and clear out) storage hashes
  $self->{sects}  = [];
  $self->{parms}  = {};
  $self->{group}  = {};
  $self->{v}      = {};
  $self->{sCMT}   = {};
  $self->{pCMT}   = {};
  $self->{EOT}    = {};
  $self->{mysects} = []; # A pair of hashes to remember which params are loaded
  $self->{myparms} = {}; # or set using the API vs. imported - useful for
  # import shadowing, see below, and WriteConfig(-delta=>1)

  if( defined $self->{imported} ) {
      # Run up the import tree to the top, then reload coming
      # back down, maintaining the imported file names and our 
      # file name.
      # This is only needed on a re-load though
	  $self->{imported}->ReadConfig() unless ($self->{firstload});

	  foreach my $field (qw(sects parms group v sCMT pCMT EOT)) {
		  $self->{$field} = _deepcopy($self->{imported}->{$field});
	  }
  } # end if
  
  return 1 if (
    (not exists $self->{cf}) or
    (not defined $self->{cf}) or
    ($self->{cf} eq '')
  );
  
  my $nocase = $self->{nocase};

  # If this is a reload and we want warnings then send one to the STDERR log
  unless( $self->{firstload} || !$self->{reloadwarn} ) {
    my ($ss, $mm, $hh, $DD, $MM, $YY) = (localtime(time))[0..5];
    printf STDERR
      "PID %d reloading config file %s at %d.%02d.%02d %02d:%02d:%02d\n",
      $$, $self->{cf}, $YY+1900, $MM+1, $DD, $hh, $mm, $ss;
  }
  
  # Turn off. Future loads are reloads
  $self->{firstload} = 0;

  # Get a filehandle, allowing almost any type of 'file' parameter
  my $fh = $self->_make_filehandle( $self->{cf} );
  if (!$fh) {
    carp "Failed to open $self->{cf}: $!";
    return undef;
  }
  
  # Get mod time of file so we can retain it (if not from STDIN)
  if (ref($fh) ne "IO::Scalar")
  {
    my @stats = stat $fh;
    $self->{file_mode} = sprintf("%04o", $stats[2]) if defined $stats[2];
  }
  
  
  # The first lines of the file must be blank, comments or start with [
  my $first = '';
  my $allCmt = $self->{allowed_comment_char};
  
  local $_;
  delete $self->{line_ends}; # Marks start of parsing for _nextline()
  while ( defined($_ = $self->_nextline($fh)) ) {
    s/(\015\012?|\012|\025|\n)$//;				# remove line ending char(s)
    $lineno++;
    if (/^\s*$/) {				# ignore blank lines
      next;
    }
    elsif (/^\s*[$allCmt]/) {			# collect comments
		if ($self->{negativedeltas} &&
			m/^$self->{comment_char} (.*) is deleted$/) {
			my $todelete=$1;
			if ($todelete =~ m/^\[(.*)\]$/) {
				$self->DeleteSection($1);
			} else {
				$self->delval($sect, $todelete);
			}
		} else {
			CORE::push(@cmts, $_);
		}
		next;
    }
    elsif (/^\s*\[\s*(\S|\S.*\S)\s*\]\s*$/) {		# New Section
      $sect = $1;
      if ($self->{nocase}) {
        $sect = lc($sect);
      }
      $self->AddSection($sect);
      $self->SetSectionComment($sect, @cmts);
      @cmts = ();
    }
    elsif (($parm, $val) = /^\s*([^=]*?[^=\s])\s*=\s*(.*)$/) {	# new parameter
		if (!defined $sect) {
			CORE::push(@Config::IniFiles::errors, sprintf('%d: %s', $lineno,
				qq#parameter found outside a section#));
			$self->_rollback($fh);
			return undef;
		}

      $parm = lc($parm) if $nocase;
      my @val = ( );
      my $eotmark;
      if ($val =~ /^<<(.*)$/) {			# "here" value
	   $eotmark  = $1;
	my $foundeot = 0;
	my $startline = $lineno;
	while ( defined($_=$self->_nextline($fh)) ) {
	  s/(\015\012?|\012|\025|\n)$//;				# remove line ending char(s)
	  $lineno++;
	  if ($_ eq $eotmark) {
	    $foundeot = 1;
	    last;
	  } else {
		# Untaint
		/(.*)/ms; 
        CORE::push(@val, $1);
	  }
	}
	if (! $foundeot) {
	  CORE::push(@Config::IniFiles::errors, sprintf('%d: %s', $startline,
			      qq#no end marker ("$eotmark") found#));
      $self->_rollback();
	  return undef;
    }
      } else { # no here value

        # process continuation lines, if any
        while($self->{allowcontinue} && $val =~ s/\\$//) {
          $_ = $self->_nextline($fh);
	  s/(\015\012?|\012|\025|\n)$//; # remove line ending char(s)
	  $lineno++;
          $val .= $_;
        }
		@val = $val;
      }
		# Now load value
		if (exists $self->{v}{$sect}{$parm} && 
			exists $self->{myparms}{$sect} && 
			grep( /^\Q$parm\E$/, @{$self->{myparms}{$sect}}) ) {
			$self->push($sect, $parm, @val);
		} else {
	        # Loaded parameters shadow imported ones, instead of appending
			# to them
			$self->newval($sect, $parm, @val);
		}
		$self->SetParameterComment($sect, $parm, @cmts);
		@cmts = ( );
		$self->SetParameterEOT($sect,$parm,$eotmark) if (defined $eotmark);

    } else {
      CORE::push(@Config::IniFiles::errors, sprintf("Line \%d in file " . $self->{cf} . " is mal-formed:\n\t\%s", $lineno, $_));
    }
  } # End main parsing loop

  # Special case: return undef if file is empty. (suppress this line to
  # restore the more intuitive behaviour of accepting empty files)
  if (! keys %{$self->{v}} && ! $self->{allowempty}) {
	  CORE::push @Config::IniFiles::errors, "Empty file treated as error";
	  $self->_rollback($fh);
	  return undef;
  }

  if( defined (my $defaultsect=$self->{startup_settings}->{-default}) ) {
	  $self->AddSection($defaultsect);
  } # end if

  $self->_SetEndComments(@cmts);

  $self->_rollback($fh);
  @Config::IniFiles::errors ? undef : 1;
}


sub Sections {
  my $self = shift;
  return @{$self->{sects}} if ref $self->{sects} eq 'ARRAY';
  return ();
}

sub SectionExists {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	return undef() if not defined $sect;
	return 1 if (grep {/^\Q$sect\E$/} @{$self->{sects}});
	return 0;
}

sub AddSection {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	return if $self->SectionExists($sect);
	CORE::push @{$self->{sects}}, $sect unless
	  grep /^\Q$sect\E$/, @{$self->{sects}};
	$self->_touch_section($sect);

	$self->SetGroupMember($sect);
	
	# Set up the parameter names and values lists
    $self->{parms}{$sect} = [] unless ref $self->{parms}{$sect} eq 'ARRAY';
	if (!defined($self->{v}{$sect})) {
		$self->{sCMT}{$sect} = [];
		$self->{pCMT}{$sect} = {};		# Comments above parameters
		$self->{parms}{$sect} = [];
		$self->{v}{$sect} = {};
	}
}

# Marks a section as modified by us (this includes deleted by us).
sub _touch_section {
	my ($self, $sect)=@_;

	$self->{mysects} ||= [];
	CORE::push @{$self->{mysects}}, $sect unless
	  grep /^\Q$sect\E$/, @{$self->{mysects}};
}

# Marks a parameter as modified by us (this includes deleted by us).
sub _touch_parameter {
	my ($self, $sect, $parm)=@_;

	$self->_touch_section($sect);
	return if (!exists $self->{v}{$sect});
	$self->{myparms}{$sect} ||= [];
	CORE::push @{$self->{myparms}{$sect}}, $parm unless
	  grep /^\Q$parm\E$/, @{$self->{myparms}{$sect}};
}


sub DeleteSection {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}

	# This is done the fast way, change if data structure changes!!
	delete $self->{v}{$sect};
	delete $self->{sCMT}{$sect};
	delete $self->{pCMT}{$sect};
	delete $self->{EOT}{$sect};
	delete $self->{parms}{$sect};
	delete $self->{myparms}{$sect};

	@{$self->{sects}} = grep !/^\Q$sect\E$/, @{$self->{sects}};
	$self->_touch_section($sect);

	if( $sect =~ /^(\S+)\s+\S+/ ) {
		my $group = $1;
		if( defined($self->{group}{$group}) ) {
			@{$self->{group}{$group}} = grep !/^\Q$sect\E$/, @{$self->{group}{$group}};
		} # end if
	} # end if

	return 1;
} # end DeleteSection

sub Parameters {
  my $self = shift;
  my $sect = shift;
  
  return undef if not defined $sect;
  
  if ($self->{nocase}) {
    $sect = lc($sect);
  }
  
  return @{$self->{parms}{$sect}} if ref $self->{parms}{$sect} eq 'ARRAY';
  return ();
}

sub Groups	{
  my $self = shift;
  return keys %{$self->{group}} if ref $self->{group} eq 'HASH';
  return ();
}

sub SetGroupMember {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	return(1) unless $sect =~ /^(\S+)\s+\S+/;
	
	my $group = $1;
	if (not exists($self->{group}{$group})) {
		$self->{group}{$group} = [];
	}
	if (not grep {/^\Q$sect\E$/} @{$self->{group}{$group}}) {
		CORE::push @{$self->{group}{$group}}, $sect;
	}
}

sub RemoveGroupMember {
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	return(1) unless $sect =~ /^(\S+)\s+\S+/;
	
	my $group = $1;
	return unless exists $self->{group}{$group};
	@{$self->{group}{$group}} = grep {!/^\Q$sect\E$/} @{$self->{group}{$group}};
}

sub GroupMembers {
  my $self  = shift;
  my $group = shift;
  
  return undef if not defined $group;
  
  if ($self->{nocase}) {
  	$group = lc($group);
  }
  
  return @{$self->{group}{$group}} if ref $self->{group}{$group} eq 'ARRAY';
  return ();
}

sub SetWriteMode
{
	my $self = shift;
	my $mode = shift;
	return undef if not defined ($mode);
	return undef if not ($mode =~ m/[0-7]{3,3}/);
	$self->{file_mode} = $mode;
	return $mode;
}

sub GetWriteMode
{
	my $self = shift;
	return undef if not exists $self->{file_mode};
	return $self->{file_mode};
}

sub WriteConfig {
  my ($self, $file, %parms)=@_;
  %parms = () unless %parms;
  
  return undef unless defined $file;
  
  
  # If we are using a filename, then do mode checks and write to a 
  # temporary file to avoid a race condition
  if( !ref($file) ) {
    if (-e $file) {
          if (not (-w $file))
          {
                  #carp "File $file is not writable.  Refusing to write config";
                  return undef;
          }
          my $mode = (stat $file)[2];
          $self->{file_mode} = sprintf "%04o", ($mode & 0777);
          #carp "Using mode $self->{file_mode} for file $file";
    } elsif (defined($self->{file_mode}) and not (oct($self->{file_mode}) & 0222)) {
          #carp "Store mode $self->{file_mode} prohibits writing config";
    }
  
    my $new_file = $file . "-new";
    local(*F);
    open(F, "> $new_file") || do {
      carp "Unable to write temp config file $new_file: $!";
      return undef;
    };
    my $oldfh = select(F);
    $self->OutputConfig($parms{-delta});
    close(F);
    select($oldfh);
    rename( $new_file, $file ) || do {
      carp "Unable to rename temp config file ($new_file) to $file: $!";
      return undef;
    };
    if (exists $self->{file_mode}) {
      chmod oct($self->{file_mode}), $file;
    }
  
  } # Otherwise, reset to the start of the file and write, unless we are using STDIN
  else {
    # Get a filehandle, allowing almost any type of 'file' parameter
    ## NB: If this were a filename, this would fail because _make_file 
    ##     opens a read-only handle, but we have already checked that case
    ##     so re-using the logic is ok [JW/WADG]
    my $fh = $self->_make_filehandle( $file );
    if (!$fh) {
      carp "Could not find a filehandle for the input stream ($file): $!";
      return undef;
    }
    
    
    # Only roll back if it's not STDIN (if it is, Carp)
    if( $fh == \*STDIN ) {
      carp "Cannot write configuration file to STDIN.";
    } else {
      seek( $fh, 0, 0 );
      my $oldfh = select($fh);
      $self->OutputConfig($parms{-delta});
      seek( $fh, 0, 0 );
      select($oldfh);
    } # end if

  } # end if (filehandle/name)
  
  return 1;
  
}

sub RewriteConfig {
  my $self = shift;
  
  return undef if (
    (not exists $self->{cf}) or
    (not defined $self->{cf}) or
    ($self->{cf} eq '')
  );
  
  # Return whatever WriteConfig returns :)
  $self->WriteConfig($self->{cf});
}

sub GetFileName
{
	my $self = shift;
	my $filename;
	if (exists $self->{cf}) {
		$filename = $self->{cf};
	} else {
		undef $filename;
	}
	return $filename;
}

sub SetFileName {
  my $self = shift;
  my $newfile = shift;
  
  return undef if not defined $newfile;
  
  if ($newfile ne "") {
    $self->{cf} = $newfile;
    return $self->{cf};
  }
  return undef;
}

sub OutputConfig {
    my ($self, $delta) = @_;

    my($sect, $parm, @cmts);
    my $ors = $self->{line_ends} || $\ || "\n"; # $\ is normally unset, but use input by default
    my $notfirst = 0;
    local $_;
    SECT:
    foreach $sect (@{$self->{$delta ? "mysects" : "sects"}}) {
        if (!defined $self->{v}{$sect}) {
            if ($delta) {
                print "$self->{comment_char} [$sect] is deleted$ors";
            } else {
                warn "Weird unknown section $sect" if $^W;
            }
            next SECT;
        }
        next unless defined $self->{v}{$sect};
        print $ors if $notfirst;
        $notfirst = 1;
        if ((ref($self->{sCMT}{$sect}) eq 'ARRAY') &&
            (@cmts = @{$self->{sCMT}{$sect}})) {
            foreach (@cmts) {
                print "$_$ors";
            }
        }
        print "[$sect]$ors";
        next unless ref $self->{v}{$sect} eq 'HASH';

        PARM:
        foreach $parm (@{$self->{$delta ? "myparms" : "parms"}{$sect}}) {
            if (!defined $self->{v}{$sect}{$parm}) {
                if ($delta) {
                    print "$self->{comment_char} $parm is deleted$ors";
                } else {
                    warn "Weird unknown parameter $parm" if $^W;
                }
                next PARM;
            }
            if ((ref($self->{pCMT}{$sect}{$parm}) eq 'ARRAY') &&
                (@cmts = @{$self->{pCMT}{$sect}{$parm}})) {
                foreach (@cmts) {
                    print "$_$ors";
                }
            }

            my $val = $self->{v}{$sect}{$parm};
            next if ! defined ($val); # No parameter exists !!
            if (ref($val) eq 'ARRAY') {
                my $eotmark = $self->{EOT}{$sect}{$parm} || 'EOT';
                print "$parm= <<$eotmark$ors";
                foreach (@{$val}) {
                    print "$_$ors";
                }
                print "$eotmark$ors";
            } elsif( $val =~ /[$ors]/ ) {
                # The FETCH of a tied hash is never called in 
                # an array context, so generate a EOT multiline
                # entry if the entry looks to be multiline
                my @val = split /[$ors]/, $val, -1;
                if( @val > 1 ) {
                    my $eotmark = $self->{EOT}{$sect}{$parm} || 'EOT';

                    # Make sure the $eotmark does not occur inside the string.
                    my @letters = ('A' .. 'Z');
                    while (index($val, $eotmark) >= 0)
                    {
                        $eotmark .= $letters[rand(@letters)];
                    }

                    print "$parm= <<$eotmark$ors";
                    print map "$_$ors", @val;
                    print "$eotmark$ors";
                } else {
                    print "$parm=$val[0]$ors";
                } # end if
            } else {
                print "$parm=$val$ors";
            }
        }
    }
    foreach my $comment ($self->_GetEndComments()) {
        print "$comment$ors";
    }
    return 1;
}

sub SetSectionComment
{
	my $self = shift;
	my $sect = shift;
	my @comment = @_;

	return undef if not defined $sect;
	return undef unless @comment;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	$self->_touch_section($sect);
	$self->{sCMT}{$sect} = [];
	# At this point it's possible to have a comment for a section that
	# doesn't exist. This comment will not get written to the INI file.
	
	CORE::push @{$self->{sCMT}{$sect}}, $self->_markup_comments(@comment);
	return scalar @comment;
}



# this helper makes sure that each line is preceded with the correct comment
# character
sub _markup_comments 
{
  my $self = shift;
  my @comment = @_;

  my $allCmt = $self->{allowed_comment_char};
  my $cmtChr = $self->{comment_char};
  foreach (@comment) {
    m/^\s*[$allCmt]/ or ($_ = "$cmtChr $_");
  }
  @comment;
}



sub GetSectionComment
{
	my $self = shift;
	my $sect = shift;

	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	
	if (exists $self->{sCMT}{$sect}) {
		my @ret = @{$self->{sCMT}{$sect}};
        if (wantarray()) {
            return @ret;
        }
        else {
            if (defined ($/)) {
                return join "$/", @ret;
            } else {
                return join "\n", @ret;
            }
        }
	} else {
		return undef;
	}
}

sub DeleteSectionComment
{
	my $self = shift;
	my $sect = shift;
	
	return undef if not defined $sect;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
	}
	$self->_touch_section($sect);

	delete $self->{sCMT}{$sect};
}

sub SetParameterComment
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	my @comment = @_;

	defined($sect) || return undef;
	defined($parm) || return undef;
	@comment || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	}
	
	$self->_touch_parameter($sect, $parm);
	if (not exists $self->{pCMT}{$sect}) {
		$self->{pCMT}{$sect} = {};
	}
	
	$self->{pCMT}{$sect}{$parm} = [];
	# Note that at this point, it's possible to have a comment for a parameter,
	# without that parameter actually existing in the INI file.
	CORE::push @{$self->{pCMT}{$sect}{$parm}}, $self->_markup_comments(@comment);
	return scalar @comment;
}

sub _SetEndComments
{
    my $self = shift;
    my @comments = @_;

    $self->{_comments_at_end_of_file} = \@comments;

    return 1;
}

sub _GetEndComments {
    my $self = shift;

    return @{$self->{_comments_at_end_of_file}};
}

sub GetParameterComment
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	
	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};
	
	exists($self->{pCMT}{$sect}) || return undef;
	exists($self->{pCMT}{$sect}{$parm}) || return undef;
	
	my @comment = @{$self->{pCMT}{$sect}{$parm}};
	return wantarray() ? @comment : join((defined $/ ? $/ : "\n"), @comment);
}

sub DeleteParameterComment
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	
	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};
	
	# If the parameter doesn't exist, our goal has already been achieved
	exists($self->{pCMT}{$sect}) || return 1;
	exists($self->{pCMT}{$sect}{$parm}) || return 1;

	$self->_touch_parameter($sect, $parm);
	delete $self->{pCMT}{$sect}{$parm};
	return 1;
}

sub GetParameterEOT
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;

	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};

	if (not exists $self->{EOT}{$sect}) {
		$self->{EOT}{$sect} = {};
	}

	if (not exists $self->{EOT}{$sect}{$parm}) {
		return undef;
	}
	return $self->{EOT}{$sect}{$parm};
}

sub SetParameterEOT
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	my $EOT = shift;

	defined($sect) || return undef;
	defined($parm) || return undef;
	defined($EOT) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	};

	$self->_touch_parameter($sect, $parm);
    if (not exists $self->{EOT}{$sect}) {
        $self->{EOT}{$sect} = {};
    }

    $self->{EOT}{$sect}{$parm} = $EOT;
}

sub DeleteParameterEOT
{
	my $self = shift;
	my $sect = shift;
	my $parm = shift;
	
	defined($sect) || return undef;
	defined($parm) || return undef;
	
	if ($self->{nocase}) {
		$sect = lc($sect);
		$parm = lc($parm);
	}

	$self->_touch_parameter($sect, $parm);
	delete $self->{EOT}{$sect}{$parm};
}


sub Delete {
	my $self = shift;

	foreach my $section ($self->Sections()) {
		$self->DeleteSection($section);
	}

	return 1;
} # end Delete



############################################################
#
# TIEHASH Methods
#
# Description:
# These methods allow you to tie a hash to the 
# Config::IniFiles object. Note that, when tied, the 
# user wants to look at thinks like $ini{sec}{parm}, but the 
# TIEHASH only provides one level of hash interace, so the 
# root object gets asked for a $ini{sec}, which this 
# implements. To further tie the {parm} hash, the internal 
# class Config::IniFiles::_section, is provided, below.
#
############################################################
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub TIEHASH {
  my $class = shift;
  my %parms = @_;

  # Get a new object
  my $self = $class->new( %parms );

  return $self;
} # end TIEHASH


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub FETCH {
  my $self = shift;
  my( $key ) = @_;

  $key = lc($key) if( $self->{nocase} );
  return if (! $self->{v}{$key});

  my %retval;
  tie %retval, 'Config::IniFiles::_section', $self, $key;
  return \%retval;

} # end FETCH

# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000Jun14 Fixed bug where wrong ref was saved           JW
# 2000Oct09 Fixed possible but in %parms with defaults    JW
# 2001Apr04 Fixed -nocase problem in storing              JW
# ----------------------------------------------------------
sub STORE {
  my $self = shift;
  my( $key, $ref ) = @_;

  return undef unless ref($ref) eq 'HASH';

  $key = lc($key) if( $self->{nocase} );

  $self->AddSection($key);
  $self->{v}{$key} = {%$ref};
  $self->{parms}{$key} = [keys %$ref];
  $self->{myparms}{$key} = [keys %$ref];
  1;
} # end STORE


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# 2000Dec17 Now removes comments, groups and EOTs too     JW
# 2001Arp04 Fixed -nocase problem                         JW
# ----------------------------------------------------------
sub DELETE {
  my $self = shift;
  my( $key ) = @_;

  my $retval=$self->FETCH($key);
  $self->DeleteSection($key);
  return $retval;
} # end DELETE


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub CLEAR {
  my $self = shift;

  return $self->Delete();
} # end CLEAR

# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub FIRSTKEY {
  my $self = shift;

  $self->{tied_enumerator}=0;
  return $self->NEXTKEY();
} # end FIRSTKEY


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub NEXTKEY {
  my $self = shift;
  my( $last ) = @_;

  my $i=$self->{tied_enumerator}++;
  my $key=$self->{sects}[$i]; 
  return if (! defined $key);
  return wantarray ? ($key, $self->FETCH($key)) : $key;
} # end NEXTKEY


# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# 2001Apr04 Fixed -nocase bug and false true bug          JW
# ----------------------------------------------------------
sub EXISTS {
  my $self = shift;
  my( $key ) = @_;
  return $self->SectionExists($key);
} # end EXISTS


# ----------------------------------------------------------
# DESTROY is used by TIEHASH and the Perl garbage collector,
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000May09 Created method                                JW
# ----------------------------------------------------------
sub DESTROY {
  # my $self = shift;
} # end if


# ----------------------------------------------------------
# Sub: _make_filehandle
#
# Args: $thing
#	$thing	An input source
#
# Description: Takes an input source of a filehandle, 
# filehandle glob, reference to a filehandle glob, IO::File
# object or scalar filename and returns a file handle to 
# read from it with.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 06Dec2001 Added to support input from any source        JW
# ----------------------------------------------------------
sub _make_filehandle {
  my $self = shift;

  #
  # This code is 'borrowed' from Lincoln D. Stein's GD.pm module
  # with modification for this module. Thanks Lincoln!
  #
  
  no strict 'refs';
  my $thing = shift;

  if (ref($thing) eq "SCALAR") {
	  if (eval { require IO::Scalar; $IO::Scalar::VERSION >= 2.109; }) {
		  return new IO::Scalar($thing);
	  } else {
		  warn "SCALAR reference as file descriptor requires IO::stringy ".
			"v2.109 or later" if ($^W);
		  return;
	  }
  }

  return $thing if defined(fileno $thing);
#  return $thing if defined($thing) && ref($thing) && defined(fileno $thing);
  
  # otherwise try qualifying it into caller's package
  my $fh = qualify_to_ref($thing,caller(1));
  return $fh if defined(fileno $fh);
#  return $fh if defined($thing) && ref($thing) && defined(fileno $fh);
  
  # otherwise treat it as a file to open
  $fh = gensym;
  open($fh,$thing) || return;
  
  return $fh;
} # end _make_filehandle

############################################################
#
# INTERNAL PACKAGE: Config::IniFiles::_section
#
# Description:
# This package is used to provide a single-level TIEHASH
# interface to the sections in the IniFile. When tied, the 
# user wants to look at thinks like $ini{sec}{parm}, but the 
# TIEHASH only provides one level of hash interace, so the 
# root object gets asked for a $ini{sec} and must return a 
# has reference that accurately covers the '{parm}' part.
#
# This package is only used when tied and is inter-woven 
# between the sections and their parameters when the TIEHASH
# method is called by Perl. It's a very simple implementation
# of a tied hash object that simply maps onto the object API.
#
############################################################
# Date        Modification                            Author
# ----------------------------------------------------------
# 2000.May.09 Created to excapsulate TIEHASH interface    JW
############################################################
package Config::IniFiles::_section;

use strict;
use Carp;
use vars qw( $VERSION );

$Config::IniFiles::_section::VERSION = 2.16;

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::TIEHASH
#
# Args: $class, $config, $section
#	$class	  The class that this is being tied to.
#   $config   The parent Config::IniFiles object
#   $section  The section this tied object refers to
#
# Description: Builds the object that implements accesses to
# the tied hash.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub TIEHASH {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my ($config, $section)=@_;

  # Make a new object
  return bless {config=>$config, section=>$section}, $class;
} # end TIEHASH


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::FETCH
#
# Args: $key
#	$key	The name of the key whose value to get
#
# Description: Returns the value associated with $key. If
# the value is a list, returns a list reference.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2000Jun15 Fixed bugs in -default handler                JW
# 2000Dec07 Fixed another bug in -deault handler          JW
# 2002Jul04 Returning scalar values (Bug:447532)          AS
# ----------------------------------------------------------
sub FETCH {
	my ($self, $key)=@_;
	my @retval=$self->{config}->val($self->{section}, $key);
	return (@retval <= 1) ? $retval[0] : \@retval;
} # end FETCH


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::STORE
#
# Args: $key, @val
#	$key	The key under which to store the value
#	@val	The value to store, either an array or a scalar
#
# Description: Sets the value for the specified $key
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2001Apr04 Fixed -nocase bug                             JW
# ----------------------------------------------------------
sub STORE {
	my ($self, $key, @val)=@_;
	return $self->{config}->newval($self->{section}, $key, @val);
} # end STORE


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::DELETE
#
# Args: $key
#	$key	The key to remove from the hash
#
# Description: Removes the specified key from the hash and
# returns its former value.
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2001Apr04 Fixed -nocase bug                              JW
# ----------------------------------------------------------
sub DELETE {
	my ($self, $key)=@_;
	my $retval=$self->{config}->val($self->{section}, $key);
	$self->{config}->delval($self->{section}, $key);
	return $retval;
} # end DELETE

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::CLEAR
#
# Args: (None)
#
# Description: Empties the entire hash
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub CLEAR    {
  my ($self) = @_;
  return $self->{config}->DeleteSection($self->{section});
} # end CLEAR

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::EXISTS
#
# Args: $key
#	$key	The key to look for
#
# Description: Returns whether the key exists
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# 2001Apr04 Fixed -nocase bug                             JW
# ----------------------------------------------------------
sub EXISTS   {
  my ($self, $key)=@_;
  return $self->{config}->exists($self->{section},$key);
} # end EXISTS

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::FIRSTKEY
#
# Args: (None)
#
# Description: Returns the first key in the hash
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub FIRSTKEY {
  my $self = shift;

  $self->{tied_enumerator}=0;
  return $self->NEXTKEY();
} # end FIRSTKEY

# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::NEXTKEY
#
# Args: $last
#	$last	The last key accessed by the interator
#
# Description: Returns the next key in line
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub NEXTKEY  {
  my $self = shift;
  my( $last ) = @_;

  my $i=$self->{tied_enumerator}++;
  my @keys = $self->{config}->Parameters($self->{section});
  my $key=$keys[$i];
  return if (! defined $key);
  return wantarray ? ($key, $self->FETCH($key)) : $key;
} # end NEXTKEY


# ----------------------------------------------------------
# Sub: Config::IniFiles::_section::DESTROY
#
# Args: (None)
#
# Description: Called on cleanup
# ----------------------------------------------------------
# Date      Modification                              Author
# ----------------------------------------------------------
# ----------------------------------------------------------
sub DESTROY  {
  # my $self = shift
} # end DESTROY

1;


1;

# Please keep the following within the last four lines of the file
#[JW for editor]:mode=perl:tabSize=8:indentSize=2:noTabs=true:indentOnEnter=true:

