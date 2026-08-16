#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

my @files = @ARGV;
if (!@files) {
  # Read from stdin if no args (allow piping)
  while (<STDIN>) { chomp; push @files, $_ if $_; }
}

# Blank out Pascal comments so prose can't be mistaken for code. A wrapped
# doc-comment line that happens to begin with "unit " (e.g. "...so this
# unit never touches...") otherwise matches before the real declaration and
# reports a mismatch against a word from a sentence.
# Comment bodies are replaced space-for-space and newlines are kept, so line
# anchoring and any reported positions still line up with the original file.
sub strip_comments {
  my ($src) = @_;
  my $out = '';
  # state: code | brace {...} | paren (*...*) | line // | string '...'
  my $state = 'code';
  my $i = 0;
  my $len = length $src;
  while ($i < $len) {
    my $c  = substr($src, $i, 1);
    my $c2 = substr($src, $i, 2);
    if ($state eq 'code') {
      if    ($c2 eq '(*') { $state = 'paren'; $out .= '  '; $i += 2; next; }
      elsif ($c2 eq '//') { $state = 'line';  $out .= '  '; $i += 2; next; }
      elsif ($c  eq '{')  { $state = 'brace'; $out .= ' ';  $i++;    next; }
      # Track string literals so an apostrophe in code can't be confused with
      # a comment delimiter, and vice versa.
      elsif ($c  eq "'")  { $state = 'string'; $out .= $c;  $i++;    next; }
      $out .= $c; $i++; next;
    }
    if ($state eq 'string') {
      $out .= $c; $i++;
      $state = 'code' if $c eq "'";   # doubled '' reopens on the next pass
      next;
    }
    # inside a comment: keep newlines, blank everything else
    if    ($state eq 'brace' && $c  eq '}')  { $state = 'code'; $out .= ' ';  $i++;    next; }
    elsif ($state eq 'paren' && $c2 eq '*)') { $state = 'code'; $out .= '  '; $i += 2; next; }
    elsif ($state eq 'line'  && $c  eq "\n") { $state = 'code'; $out .= "\n"; $i++;    next; }
    $out .= ($c eq "\n") ? "\n" : ' ';
    $i++;
  }
  return $out;
}

my $errors = 0;
for my $f (@files) {
  next unless -f $f;
  next if $f =~ m{(?:^|/)backup/};
  open my $fh, '<', $f or next;
  my $src = do { local $/; <$fh> };
  close $fh;
  my $unit;
  if (strip_comments($src) =~ /^\s*unit\s+([A-Za-z0-9_.]+)/im) { $unit = $1; }
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
