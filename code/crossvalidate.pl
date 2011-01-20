#!/usr/bin/perl

# K-Fold Cross-Validater
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
use Getopt::Std;
use POSIX;
use List::Util 'shuffle';

local $| = 1;
our ($opt_k,$opt_p,$opt_e,$opt_t,$opt_v,$opt_d,$opt_u,$opt_r);
my $usage = "\nK-Fold Cross-Validater\n\n" . 
			"Usage: ./crossvalidate.pl [OPTIONS] SEGMENTATION_COMMAND TRUE_CORPUS\n\n" . 
			"SEGMENTATION_COMMAND = A string (enclosed by quotes) that specifies the segmenter to run" .
			" (with arguments). The string should end with the 'supervisedFor' argument for the segmenter" .
			" (without the number specified, as it will be passed in by this script).\n" .
			"TRUE_CORPUS = The correctly segmented reference corpus.\n" .
			"Options:\n" . 
			"\t-e FLAGS\tFlags to pass on to errors.pl. (Must enclose in quotes.)\n" .
			"\t-k NUM_FOLDS\tSpecifies number of folds for cross-validation. (Default = 10)\n" .
			"\t-p PREFIX\tFile prefix for temporary fold files. Note: All files with this prefix are DELETED when script ends. (Default = '.cvfoldtemp')\n" .
			"\t-t TEST_FOLDS\tNumber of folds to test on.  (Default = 1, cannot specify 0)\n" .
			"\t-u UNSUPERVISED_FOLDS\tNumber of folds to exclude from testing and supervised training. (Default = 0)\n" .
			"\t-d \t\tDo NOT delete files that begin with PREFIX when done. (Although any existing ones are still destroyed when the script is first run.)\n" .
			"\t-r \t\tRandomize order of test folds.\n" .
			"\t-v \t\tOutput result of running errors.pl for each individual fold.\n";

# Handle arguments
getopts('e:k:p:t:u:dvr');
die $usage if @ARGV < 2;
my $segmenter = shift @ARGV;
my $corpus = shift @ARGV;

my $prefix = ".cvfoldtemp";
my $folds = 10;

if ($opt_p)
{
	$prefix = $opt_p;
}
$prefix = $prefix . time();

if ($opt_k)
{
	$folds = $opt_k;
}

my $trainingFolds = $folds;
my $testFolds = 1; 

if ($opt_t)
{
	$testFolds = $opt_t;
}

$trainingFolds -= $testFolds; # Exclude test folds from training.

my $unsupervisedFolds = $opt_u;

my @allFiles = <$prefix*>;
if (scalar(@allFiles) > 0)
{
	`rm $prefix*`;
}
my $corpusLength = `wc -l $corpus | perl -p -e 's/^[ \\t]+([0-9]+)[ \\t].*\$/\$1/'`;
my $foldSize = floor($corpusLength / $folds);
my $suffixLength = ceil(log($folds) / log(26));
`split -a $suffixLength -l $foldSize $corpus $prefix`;
my %training;
my @test;
my $ir;
@allFiles = <$prefix*>;
my $trainingSize = $foldSize * $trainingFolds;
my $testSize = $corpusLength - $trainingSize;
$trainingSize -= $foldSize * $unsupervisedFolds; # Remove unsupervised training folds from training size calculation
my @currentFiles;
my $trainingIndex;
for (my $i = 0; $i < $folds; $i++)
{
	@currentFiles = @allFiles;
	%training = ();
	@test = ();
	if ($i > 0)
	{
		`rm $prefix-gold-current`;		
	}
	for (my $j = 0; $j < $trainingFolds; $j++)
	{
		$trainingIndex = ($j + $i) % $folds;
		$training{$trainingIndex} = 1;
		`cat $allFiles[$trainingIndex] >> $prefix-gold-current`;
	}
	for (my $j = 0; $j < $folds; $j++)
	{			
		if (!$training{$j})
		{
			push @test, $currentFiles[$j];
		}
	}
	# Randomize test folds list
	if ($opt_r)
	{
		@test = shuffle(@test);
	}
	foreach my $f (@test)
	{
		`cat $f >> $prefix-gold-current`;
	}

	print "Segmenting fold $i with command '$segmenter $trainingSize'...";
	`$segmenter $trainingSize $prefix-gold-current | tail -n $testSize >> $prefix-results`;		
	`tail -n $testSize $prefix-gold-current >> $prefix-gold`;
	print "done\n";

	if ($opt_v)
	{
		print `./errors.pl $opt_e $prefix-gold $prefix-results`;
		`rm $prefix-gold`;
		`rm $prefix-results`;
	}
}
if (!$opt_v)
{
	print `./errors.pl $opt_e $prefix-gold $prefix-results`;			
}
if (!$opt_d)
{
	`rm $prefix*`;	
}
