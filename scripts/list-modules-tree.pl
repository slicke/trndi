#!/usr/bin/env perl
use strict;
use warnings;

my $root = {};
while (<STDIN>) {
  chomp;
  # input is: <unit.name>\t<filepath>
  my ($u, $path) = split(/\t/, $_, 2);
  next unless defined $u && $u ne '';

  # Files physically located under `units/forms/` should be shown grouped
  # under the literal top-level label `forms*` so `make list-modules` prints a
  # `forms*` node even when the Pascal `unit` declaration has no namespace.
  if (defined $path && $path =~ m{^units[\\/](?:forms)(?:[\\/]|$)}i) {
    # don't double-prefix if the unit already starts with "forms." or "forms*."
    $u = "forms*.$u" unless $u =~ /^forms(\*|\.)/i;
  }

  my @p = split(/\./, $u);
  my $h = $root;
  for my $part (@p) {
    $h->{$part} ||= {};
    $h = $h->{$part};
  }
}

sub printnode {
  my ($h, $level) = @_;
  for my $k (sort keys %$h) {
    print ( ('  ' x $level) . ($level ? '- ' : '') . $k . "\n" );
    printnode($h->{$k}, $level+1);
  }
}

for my $k (sort keys %$root) {
  print "$k\n";
  printnode($root->{$k}, 1);
}
