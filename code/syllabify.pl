#!/usr/bin/perl

# Dan Blanchard
# Corpus Syllabifier

use strict;

my %vowels;
my %syllabic;
my $line;
my $phone;
my $seenVowel = 0;
my @syllables;
my $notDone = 1;

# Initialize Hashes
$vowels{"&"} = $vowels{"6"} = $vowels{"9"} = $vowels{"A"} = $vowels{"E"} = $vowels{"I"} = $vowels{"O"} = $vowels{"U"} = $vowels{"a"} = $vowels{"e"} = $vowels{"i"} = $vowels{"o"} = $vowels{"u"} = 1;
# $vowels{"9I"} = $vowels{"9U"} = $vowels{"OI"} = 1; # diphthongs
$syllabic{"R"} = $syllabic{"~"} = $syllabic{"M"} = $syllabic{"L"} = 1;

while ($line = <>)
{
	@syllables = ();
	chomp $line;
	for (my $i = 0; $i < length($line); $i++) 
	{
		$phone = substr $line, $i, 1;
		if (!$seenVowel)
		{
			if (exists $vowels{$phone})
			{
				$seenVowel = 1;
				push @syllables, 1;
			}
			else
			{
				$seenVowel = 0;
				push @syllables, ((exists $syllabic{$phone}) ? 1 : 0);
			}
		}
		elsif (exists $vowels{$phone})
		{
			$seenVowel = 0;
			push @syllables, 2;
		}
		else
		{
			$seenVowel = 0;
			push @syllables, ((exists $syllabic{$phone}) ? 1 : 0);
		}
	}
	print "line: $line\tsyllables: @syllables\n";
	
	while ($notDone)
	{
		for (my $i = 0; $i < expression; $i++) 
		{
			if ($vowel)
		}		
	}
}
