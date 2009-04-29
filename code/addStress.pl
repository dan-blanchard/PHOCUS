#!/usr/bin/perl

# Dan Blanchard
# Looks up stress info in CMU-dict and adds it back into corpus

use strict;
use POSIX;
use utf8;
use Encode;

my %cmuDict;
my @dictPair;
my @phonWords;
my @orthWords;
my @cmuPhones;
my $phonLine;
my $orthLine;
my $phonWord;
my $orthWord;
my $stressedWord;
my $wordDelimiter = " ";

# Setup Unicode input and output
binmode STDOUT, ":utf8";
binmode STDIN, ":utf8";
binmode STDERR, ":utf8";

my $usage = "\nUsage: ./dict-helper.pl CMU-DICT-FILE ORTHOGRAPHIC-CORPUS [PHONETIC-CORPUS]\n\n";
die $usage if @ARGV < 2;
my $dictFile = shift @ARGV;
my $orthFile = shift @ARGV;

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

open(ORTHFILE, $orthFile);
binmode ORTHFILE, ":utf8";
while ($phonLine = <>)
{
	chomp $phonLine;
	$orthLine = <ORTHFILE>;
	chomp $orthLine;
	@phonWords = split /\Q$wordDelimiter\E/,$phonLine;
	@orthWords = split /\Q$wordDelimiter\E/,$orthLine;
	for (my $i = 0; $i < scalar(@orthWords); $i++) 	
	{
		$stressedWord = "";
		$orthWord = uc($orthWords[$i]);
		$phonWord = $phonWords[$i];
		if (exists $cmuDict{$orthWord})
		{
			$stressedWord = $cmuDict{$orthWord};
		}
		else
		{
			$orthWord =~ s/'S$/S/;
			$stressedWord = $cmuDict{$orthWord};
		}
		if (!$stressedWord)
		{
			die "Word not found: $orthWord\n";
		}
		else
		{
			$stressedWord =~ s/[02]//g; # Remove unstressed and secondary-stress symbols
			@cmuPhones = split / /,$stressedWord;
			if (length($phonWord) == scalar(@cmuPhones))
			{
				for (my $j = 0; $j < scalar(@cmuPhones); $j++) 
				{
					if ($cmuPhones[$j] =~ m/1/)
					{
						substr($phonWord, $j+1, 0) = '\'';
						last; # Can stop, since words have at most one primary stress
					}
				}
				
			}
			print "$phonWord$wordDelimiter";
		}
	}
	print "\n";
}
close(ORTHFILE);
