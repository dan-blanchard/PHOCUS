#!/usr/bin/perl

# Corpus Syllabifier
# Copyright (C) 2007-2010 Dan Blanchard.
# 
# This file is part of PHOCUS.
# 
# PHOCUS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#     
# PHOCUS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#     
# You should have received a copy of the GNU General Public License
# along with PHOCUS.  If not, see <http://www.gnu.org/licenses/>.


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
	if ($syllables =~ m/[&69AEIOUaeiouR~ML]/)
	{
		$syllables =~ s/(9I)|(9U)|(OI)/--/g;  # diphthongs
		$syllables =~ s/[&69AEIOUaeiouR~ML]/length($`)%10/eg; # vowels
		$syllables =~ s/--/(length($`)%10) x 2/eg;  # diphthongs
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
	}
	print "$line\n";
}