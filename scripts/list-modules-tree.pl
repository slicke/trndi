#!/usr/bin/env perl
use strict;
use warnings;

my $root = {};
while (<STDIN>) {
  chomp;
  my ($u) = split(/\t/, $_, 2);
  next unless defined $u && $u ne '';
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
