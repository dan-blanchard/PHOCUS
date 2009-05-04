#!/usr/bin/perl

# Dan Blanchard
# Corpus Syllabifier

# TODO: Change vowels/diphthongs so they aren't hard-coded for BR corpus.  Should use feature chart to just check for +syllabic

use strict;

my $line;
my $prevSyllable;
my $currSyllable;
my $syllables;
my @breaks = ();

while ($line = <>)
{
	@breaks = ();
	chomp $line;
	$syllables = $line;
	$syllables =~ s/(9I)|(9U)|(OI)/--/g;  # diphthongs
	$syllables =~ s/[&69AEIOUaeiouR~ML]/length($`)%10/eg; # vowels
	$syllables =~ s/--/(length($`)%10) . (length($`)%10)/eg;  # diphthongs
	# Loop until we've assigned all characters to a syllable
	while ($syllables =~ m/[^0-9]/)
	{
		$syllables =~ s/[^0-9>](0+|1+|2+|3+|4+|5+|6+|7+|8+|9+)/<$1/g; # find onsets
		$syllables =~ s/(0+|1+|2+|3+|4+|5+|6+|7+|8+|9+)[^0-9<]/$1>/g; # find codas
		$syllables =~ s/<([0-9])/$1$1/g;
		$syllables =~ s/([0-9])>/$1$1/g;
	}
	
	# Find syllable break positions
	$prevSyllable = substr($syllables,0,1);
	for (my $i = 1; $i < length($syllables); $i++) 
	{
		$currSyllable = substr($syllables,$i,1);
		if ($currSyllable != $prevSyllable)
		{
			push @breaks, $i;
			$prevSyllable = $currSyllable;
			
		}
	}
	
	# Insert syllable boundaries
	for (my $i = 0; $i < scalar(@breaks); $i++) 
	{
		substr($line,$breaks[$i]+$i,0,".");
	}
	
	print "$line\n";
}