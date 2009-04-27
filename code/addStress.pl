#!/usr/bin/perl

# Dan Blanchard
# Looks up stress info in CMU-dict and adds it back into corpus

use strict;
use POSIX;
use utf8;
use Encode;

my %cmuDict;
my @dictPair;
my @line;
my $stressedWord;

# Setup Unicode input and output
binmode STDOUT, ":utf8";
binmode STDIN, ":utf8";
binmode STDERR, ":utf8";

my $usage = "\nUsage: ./dict-helper.pl CMU-DICT-FILE [ORTHOGRAPHIC-CORPORA]\n\n";
die $usage if @ARGV < 1;
my $dictFile = shift @ARGV;

open(DICTFILE, $dictFile);
binmode DICTFILE, ":utf8";
while (<DICTFILE>)
{
	chomp;
	if (($_ !~ m/[(){};"#%&!]/) && ($_ !~ m/^['.\-,\/?:]/)) # Ignore lines that start with junk (or are alternative pronunciations)
	{
		@dictPair = split /  /,$_;
		$cmuDict{$dictPair[0]} = $dictPair[1];
	}	
}
close(DICTFILE);

while (<>)
{
	chomp;
	@line = split / /,$_;
	foreach my $word (@line)
	{
		$word = uc($word);
		if (exists $cmuDict{$word})
		{
			$stressedWord = $cmuDict{$word};
		}
		elsif ($word =~ m/'S$/)
		{
			$word =~ s/'S$/S/;
			if (exists $cmuDict{$word})
			{
				$stressedWord = $cmuDict{$word};
			}
			else
			{
				die "Word not found: $word\n";
			}
		}
		else
		{
			die "Word not found: $word\n";
		}
		$stressedWord =~ s/[02]//g; # Remove unstressed and secondary-stress symbols
		print "$stressedWord#"
	}
	print "\n";
}