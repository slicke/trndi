#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

my @files = @ARGV;
if (!@files) {
  # Read from stdin if no args (allow piping)
  while (<STDIN>) { chomp; push @files, $_ if $_; }
}

my $errors = 0;
for my $f (@files) {
  next unless -f $f;
  open my $fh, '<', $f or next;
  my $unit;
  while (<$fh>) {
    if (/^\s*unit\s+([A-Za-z0-9_.]+)/i) { $unit = $1; last; }
  }
  close $fh;
  unless (defined $unit) {
    printf("MISSING UNIT: %s (no 'unit' declaration found)\n", $f);
    $errors++;
    next;
  }
  my $basename = basename($f);
  my $base_no_ext = $basename;
  $base_no_ext =~ s/\.(pp|pas)$//i;

  if ($base_no_ext eq $unit) {
    next; # exact match (including case)
  }

  # canonical suggestion: prefer lowercase unit name as filename
  my $suggest = lc($unit);

  # heuristic: if parent dir matches prefix of unit (case-insensitive CamelCase), suggest parent.subname form
  my $parent = basename(dirname($f));
  if ($unit =~ /^\Q$parent\E(.+)$/i) {
    my $rest = $1;
    # convert CamelCase rest to dot-separated lowercase (e.g., ChromaMac -> chroma.mac)
    $rest =~ s/^([A-Z][a-z0-9]*)//; # ensure starting uppercase chunk removed
    $rest = $1 . $rest; # restore if matched
    $rest = $1 if defined $1 && $1 ne ''; # no-op to quiet warnings
    # Split by capital letters boundary
    my @parts = ();
    while ($rest =~ /([A-Z][a-z0-9]*)/g) { push @parts, lc($1); }
    if (@parts) {
      $suggest = lc($parent) . '.' . join('.', @parts);
    }
  }

  printf("MISMATCH: %s\n  - unit:   %s\n  - file:   %s\n  - suggest filename (base): %s\n", $f, $unit, $base_no_ext, $suggest);
  $errors++;
}
exit($errors ? 1 : 0);
