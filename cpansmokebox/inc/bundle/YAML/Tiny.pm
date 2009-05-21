package YAML::Tiny;

use strict;
BEGIN {
	require 5.004;
	require Exporter;
	$YAML::Tiny::VERSION   = '1.36';
	$YAML::Tiny::errstr    = '';
	@YAML::Tiny::ISA       = qw{ Exporter  };
	@YAML::Tiny::EXPORT = qw{
		Load Dump
	};
	@YAML::Tiny::EXPORT_OK = qw{
		LoadFile DumpFile
		freeze   thaw
	};
}

my $ESCAPE_CHAR = '[\\x00-\\x08\\x0b-\\x0d\\x0e-\\x1f\"\n]';

# Escapes for unprintable characters
my @UNPRINTABLE = qw(
	z    x01  x02  x03  x04  x05  x06  a
	x08  t    n    v    f    r    x0e  x0f
	x10  x11  x12  x13  x14  x15  x16  x17
	x18  x19  x1a  e    x1c  x1d  x1e  x1f
);

# Printable characters for escapes
my %UNESCAPES = (
	z => "\x00", a => "\x07", t    => "\x09",
	n => "\x0a", v => "\x0b", f    => "\x0c",
	r => "\x0d", e => "\x1b", '\\' => '\\',
);

# Create an empty YAML::Tiny object
sub new {
	my $class = shift;
	bless [ @_ ], $class;
}

# Create an object from a file
sub read {
	my $class = ref $_[0] ? ref shift : shift;

	# Check the file
	my $file = shift or return $class->_error( 'You did not specify a file name' );
	return $class->_error( "File '$file' does not exist" )              unless -e $file;
	return $class->_error( "'$file' is a directory, not a file" )       unless -f _;
	return $class->_error( "Insufficient permissions to read '$file'" ) unless -r _;

	# Slurp in the file
	local $/ = undef;
	local *CFG;
	unless ( open(CFG, $file) ) {
		return $class->_error( "Failed to open file '$file': $!" );
	}
	my $contents = <CFG>;
	unless ( close(CFG) ) {
		return $class->_error( "Failed to close file '$file': $!" );
	}

	$class->read_string( $contents );
}

# Create an object from a string
sub read_string {
	my $class = ref $_[0] ? ref shift : shift;
	my $self  = bless [], $class;

	# Handle special cases
	return undef unless defined $_[0];
	return $self unless length $_[0];
	unless ( $_[0] =~ /[\012\015]+$/ ) {
		return $class->_error("Stream does not end with newline character");
	}

	# Split the file into lines
	my @lines = grep { ! /^\s*(?:\#.*)?$/ }
	            split /(?:\015{1,2}\012|\015|\012)/, shift;

	# Strip the initial YAML header
	@lines and $lines[0] =~ /^\%YAML[: ][\d\.]+.*$/ and shift @lines;

	# A nibbling parser
	while ( @lines ) {
		# Do we have a document header?
		if ( $lines[0] =~ /^---\s*(?:(.+)\s*)?$/ ) {
			# Handle scalar documents
			shift @lines;
			if ( defined $1 and $1 !~ /^(?:\#.+|\%YAML[: ][\d\.]+)$/ ) {
				push @$self, $self->_read_scalar( "$1", [ undef ], \@lines );
				next;
			}
		}

		if ( ! @lines or $lines[0] =~ /^(?:---|\.\.\.)/ ) {
			# A naked document
			push @$self, undef;
			while ( @lines and $lines[0] !~ /^---/ ) {
				shift @lines;
			}

		} elsif ( $lines[0] =~ /^\s*\-/ ) {
			# An array at the root
			my $document = [ ];
			push @$self, $document;
			$self->_read_array( $document, [ 0 ], \@lines );

		} elsif ( $lines[0] =~ /^(\s*)\S/ ) {
			# A hash at the root
			my $document = { };
			push @$self, $document;
			$self->_read_hash( $document, [ length($1) ], \@lines );

		} else {
			die "YAML::Tiny does not support the line '$lines[0]'";
		}
	}

	$self;
}

# Deparse a scalar string to the actual scalar
sub _read_scalar {
	my ($self, $string, $indent, $lines) = @_;

	# Trim trailing whitespace
	$string =~ s/\s*$//;

	# Explitic null/undef
	return undef if $string eq '~';

	# Quotes
	if ( $string =~ /^\'(.*?)\'$/ ) {
		return '' unless defined $1;
		my $rv = $1;
		$rv =~ s/\'\'/\'/g;
		return $rv;
	}
	if ( $string =~ /^\"((?:\\.|[^\"])*)\"$/ ) {
		my $str = $1;
		$str =~ s/\\"/"/g;
		$str =~ s/\\([never\\fartz]|x([0-9a-fA-F]{2}))/(length($1)>1)?pack("H2",$2):$UNESCAPES{$1}/gex;
		return $str;
	}

	# Special cases
	die "Unsupported YAML feature" if $string =~ /^['"!&]/;
	return {} if $string eq '{}';
	return [] if $string eq '[]';

	# Regular unquoted string
	return $string unless $string =~ /^[>|]/;

	# Error
	die "Multi-line scalar content missing" unless @$lines;

	# Check the indent depth
	$lines->[0] =~ /^(\s*)/;
	$indent->[-1] = length("$1");
	if ( defined $indent->[-2] and $indent->[-1] <= $indent->[-2] ) {
		die "Illegal line indenting";
	}

	# Pull the lines
	my @multiline = ();
	while ( @$lines ) {
		$lines->[0] =~ /^(\s*)/;
		last unless length($1) >= $indent->[-1];
		push @multiline, substr(shift(@$lines), length($1));
	}

	my $j = (substr($string, 0, 1) eq '>') ? ' ' : "\n";
	my $t = (substr($string, 1, 1) eq '-') ? '' : "\n";
	return join( $j, @multiline ) . $t;
}

# Parse an array
sub _read_array {
	my ($self, $array, $indent, $lines) = @_;

	while ( @$lines ) {
		# Check for a new document
		if ( $lines->[0] =~ /^(?:---|\.\.\.)/ ) {
			while ( @$lines and $lines->[0] !~ /^---/ ) {
				shift @$lines;
			}
			return 1;
		}

		# Check the indent level
		$lines->[0] =~ /^(\s*)/;
		if ( length($1) < $indent->[-1] ) {
			return 1;
		} elsif ( length($1) > $indent->[-1] ) {
			die "Hash line over-indented";
		}

		if ( $lines->[0] =~ /^(\s*\-\s+)[^\'\"]\S*\s*:(?:\s+|$)/ ) {
			# Inline nested hash
			my $indent2 = length("$1");
			$lines->[0] =~ s/-/ /;
			push @$array, { };
			$self->_read_hash( $array->[-1], [ @$indent, $indent2 ], $lines );

		} elsif ( $lines->[0] =~ /^\s*\-(\s*)(.+?)\s*$/ ) {
			# Array entry with a value
			shift @$lines;
			push @$array, $self->_read_scalar( "$2", [ @$indent, undef ], $lines );

		} elsif ( $lines->[0] =~ /^\s*\-\s*$/ ) {
			shift @$lines;
			unless ( @$lines ) {
				push @$array, undef;
				return 1;
			}
			if ( $lines->[0] =~ /^(\s*)\-/ ) {
				my $indent2 = length("$1");
				if ( $indent->[-1] == $indent2 ) {
					# Null array entry
					push @$array, undef;
				} else {
					# Naked indenter
					push @$array, [ ];
					$self->_read_array( $array->[-1], [ @$indent, $indent2 ], $lines );
				}

			} elsif ( $lines->[0] =~ /^(\s*)\S/ ) {
				push @$array, { };
				$self->_read_hash( $array->[-1], [ @$indent, length("$1") ], $lines );

			} else {
				die "YAML::Tiny does not support the line '$lines->[0]'";
			}

		} elsif ( defined $indent->[-2] and $indent->[-1] == $indent->[-2] ) {
			# This is probably a structure like the following...
			# ---
			# foo:
			# - list
			# bar: value
			#
			# ... so lets return and let the hash parser handle it
			return 1;

		} else {
			die "YAML::Tiny does not support the line '$lines->[0]'";
		}
	}

	return 1;
}

# Parse an array
sub _read_hash {
	my ($self, $hash, $indent, $lines) = @_;

	while ( @$lines ) {
		# Check for a new document
		if ( $lines->[0] =~ /^(?:---|\.\.\.)/ ) {
			while ( @$lines and $lines->[0] !~ /^---/ ) {
				shift @$lines;
			}
			return 1;
		}

		# Check the indent level
		$lines->[0] =~ /^(\s*)/;
		if ( length($1) < $indent->[-1] ) {
			return 1;
		} elsif ( length($1) > $indent->[-1] ) {
			die "Hash line over-indented";
		}

		# Get the key
		unless ( $lines->[0] =~ s/^\s*([^\'\" ][^\n]*?)\s*:(\s+|$)// ) {
			die "Unsupported YAML feature" if $lines->[0] =~ /^\s*[?'"]/;
			die "Bad or unsupported hash line";
		}
		my $key = $1;

		# Do we have a value?
		if ( length $lines->[0] ) {
			# Yes
			$hash->{$key} = $self->_read_scalar( shift(@$lines), [ @$indent, undef ], $lines );
		} else {
			# An indent
			shift @$lines;
			unless ( @$lines ) {
				$hash->{$key} = undef;
				return 1;
			}
			if ( $lines->[0] =~ /^(\s*)-/ ) {
				$hash->{$key} = [];
				$self->_read_array( $hash->{$key}, [ @$indent, length($1) ], $lines );
			} elsif ( $lines->[0] =~ /^(\s*)./ ) {
				my $indent2 = length("$1");
				if ( $indent->[-1] >= $indent2 ) {
					# Null hash entry
					$hash->{$key} = undef;
				} else {
					$hash->{$key} = {};
					$self->_read_hash( $hash->{$key}, [ @$indent, length($1) ], $lines );
				}
			}
		}
	}

	return 1;
}

# Save an object to a file
sub write {
	my $self = shift;
	my $file = shift or return $self->_error(
		'No file name provided'
		);

	# Write it to the file
	open( CFG, '>' . $file ) or return $self->_error(
		"Failed to open file '$file' for writing: $!"
		);
	print CFG $self->write_string;
	close CFG;

	return 1;
}

# Save an object to a string
sub write_string {
	my $self = shift;
	return '' unless @$self;

	# Iterate over the documents
	my $indent = 0;
	my @lines  = ();
	foreach my $cursor ( @$self ) {
		push @lines, '---';

		# An empty document
		if ( ! defined $cursor ) {
			# Do nothing

		# A scalar document
		} elsif ( ! ref $cursor ) {
			$lines[-1] .= ' ' . $self->_write_scalar( $cursor, $indent );

		# A list at the root
		} elsif ( ref $cursor eq 'ARRAY' ) {
			unless ( @$cursor ) {
				$lines[-1] .= ' []';
				next;
			}
			push @lines, $self->_write_array( $cursor, $indent, {} );

		# A hash at the root
		} elsif ( ref $cursor eq 'HASH' ) {
			unless ( %$cursor ) {
				$lines[-1] .= ' {}';
				next;
			}
			push @lines, $self->_write_hash( $cursor, $indent, {} );

		} else {
			Carp::croak("Cannot serialize " . ref($cursor));
		}
	}

	join '', map { "$_\n" } @lines;
}

sub _write_scalar {
	my $str = $_[1];
	return '~'  unless defined $str;
	if ( $str =~ /$ESCAPE_CHAR/ ) {
		$str =~ s/\\/\\\\/g;
		$str =~ s/"/\\"/g;
		$str =~ s/\n/\\n/g;
		$str =~ s/([\x00-\x1f])/\\$UNPRINTABLE[ord($1)]/g;
		return qq{"$str"};
	}
	if ( length($str) == 0 or $str =~ /(?:^\W|\s)/ ) {
		$str =~ s/\'/\'\'/;
		return "'$str'";
	}
	return $str;
}

sub _write_array {
	my ($self, $array, $indent, $seen) = @_;
	if ( $seen->{refaddr($array)}++ ) {
		die "YAML::Tiny does not support circular references";
	}
	my @lines  = ();
	foreach my $el ( @$array ) {
		my $line = ('  ' x $indent) . '-';
		my $type = ref $el;
		if ( ! $type ) {
			$line .= ' ' . $self->_write_scalar( $el, $indent + 1 );
			push @lines, $line;

		} elsif ( $type eq 'ARRAY' ) {
			if ( @$el ) {
				push @lines, $line;
				push @lines, $self->_write_array( $el, $indent + 1, $seen );
			} else {
				$line .= ' []';
				push @lines, $line;
			}

		} elsif ( $type eq 'HASH' ) {
			if ( keys %$el ) {
				push @lines, $line;
				push @lines, $self->_write_hash( $el, $indent + 1, $seen );
			} else {
				$line .= ' {}';
				push @lines, $line;
			}

		} else {
			die "YAML::Tiny does not support $type references";
		}
	}

	@lines;
}

sub _write_hash {
	my ($self, $hash, $indent, $seen) = @_;
	if ( $seen->{refaddr($hash)}++ ) {
		die "YAML::Tiny does not support circular references";
	}
	my @lines  = ();
	foreach my $name ( sort keys %$hash ) {
		my $el   = $hash->{$name};
		my $line = ('  ' x $indent) . "$name:";
		my $type = ref $el;
		if ( ! $type ) {
			$line .= ' ' . $self->_write_scalar( $el, $indent + 1 );
			push @lines, $line;

		} elsif ( $type eq 'ARRAY' ) {
			if ( @$el ) {
				push @lines, $line;
				push @lines, $self->_write_array( $el, $indent + 1, $seen );
			} else {
				$line .= ' []';
				push @lines, $line;
			}

		} elsif ( $type eq 'HASH' ) {
			if ( keys %$el ) {
				push @lines, $line;
				push @lines, $self->_write_hash( $el, $indent + 1, $seen );
			} else {
				$line .= ' {}';
				push @lines, $line;
			}

		} else {
			die "YAML::Tiny does not support $type references";
		}
	}

	@lines;
}

# Set error
sub _error {
	$YAML::Tiny::errstr = $_[1];
	undef;
}

# Retrieve error
sub errstr {
	$YAML::Tiny::errstr;
}





#####################################################################
# YAML Compatibility

sub Dump {
	YAML::Tiny->new(@_)->write_string;
}

sub Load {
	my $self = YAML::Tiny->read_string(@_)
		or Carp::croak("Failed to load YAML document from string");
	if ( wantarray ) {
		return @$self;
	} else {
		# To match YAML.pm, return the last document
		return $self->[-1];
	}
}

BEGIN {
	*freeze = *Dump;
	*thaw   = *Load;
}

sub DumpFile {
	my $file = shift;
	YAML::Tiny->new(@_)->write($file);
}

sub LoadFile {
	my $self = YAML::Tiny->read($_[0])
		or Carp::croak("Failed to load YAML document from '" . ($_[0] || '') . "'");
	if ( wantarray ) {
		return @$self;
	} else {
		# To match YAML.pm, return the last document
		return $self->[-1];
	}
}





#####################################################################
# Use Scalar::Util if possible, otherwise emulate it

BEGIN {
	eval {
		require Scalar::Util;
	};
	if ( $@ ) {
		# Failed to load Scalar::Util
		eval <<'END_PERL';
sub refaddr {
	my $pkg = ref($_[0]) or return undef;
	if (!!UNIVERSAL::can($_[0], 'can')) {
		bless $_[0], 'Scalar::Util::Fake';
	} else {
		$pkg = undef;
	}
	"$_[0]" =~ /0x(\w+)/;
	my $i = do { local $^W; hex $1 };
	bless $_[0], $pkg if defined $pkg;
	$i;
}
END_PERL
	} else {
		Scalar::Util->import('refaddr');
	}
}

1;

__END__

