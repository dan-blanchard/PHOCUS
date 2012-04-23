#!/usr/bin/perl

# Corpus Analyzer
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

use strict;

my %lexicon;
my @ngramCountsArray = ();
my $wordDelimiter = " ";
my @words = ();

my $line;
my $utterance;
while ($line = <>)
{
	chomp $line;
	$utterance = $line;
	$utterance =~ s/^\Q$wordDelimiter\E*(.+[^\Q$wordDelimiter\E])\Q$wordDelimiter\E*/\1/;
	@words = split /\Q$wordDelimiter\E/,$utterance;
	foreach my $word (@words)
	{
		$lexicon{$word} = 1;
	}
	print scalar(keys %lexicon) . "\n";	
}