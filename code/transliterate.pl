#!/usr/bin/perl

# Dan Blanchard
# Corpus transliterater

use strict;
use POSIX;
use utf8;
use Encode;

my @pairs;
my @trPair;
my $line;

# Setup Unicode input and output
binmode STDOUT, ":utf8";
binmode STDIN, ":utf8";
binmode STDERR, ":utf8";

my $usage = "\nUsage: ./transliterate.pl KEY-FILE [FILES-TO-TRANSLITERATE]\n\n";
die $usage if @ARGV < 1;
my $keyFile = shift @ARGV;


open(KEYFILE, $keyFile);
binmode KEYFILE, ":utf8";
while (<KEYFILE>)
{
	chomp;
	@trPair = split /\t/,$_;
	push @pairs, [@trPair];
}
close(KEYFILE);

while (<>)
{
	$line = $_;
	for (my $i = 0; $i < scalar(@pairs); $i++) 
	{
		$line =~ s/$pairs[$i][0]/$pairs[$i][1]/g;
	}
	print $line;
}