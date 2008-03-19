#!/usr/bin/perl

# Dan Blanchard
# MBDP Implementation

# usage: ./brent.pl [-v] [-n] [-w WINDOW_SIZE] [-f FEATURE_CHART] FILE


use Math::Trig;
use strict;
use Getopt::Std;
use FeatureChart;
use List::Util qw(first max maxstr min minstr reduce shuffle sum);

our ($opt_v, $opt_n, $opt_w, $opt_f);
my $window = 1;
my @segmentation;
my @bestProduct;
my @bestStart;
my $wordScore;
my $scoreProduct;
my $segmentedSentence;
my $delimiter = " ";			# word delimiter
my %lexicon = ();
my %phonemeCounts = ();			# This stores phoneme counts (be they phonemes, phoneme n-grams, or feature n-grams)
my $totalWords = 0;
my $totalPhonemes = 0;
my @words;
my $firstChar;
my @segmentation;
my $segmentedSentence;
my %wordPhonemeCounts = ();		# Phoneme counts for novel word
my $wordTotalPhonemes = 0;		# Total phonemes for novel word
my $featureChart;
my @phoneFeatures;
my %productCache = ();
my @subList;
my @concatenatedResults;
my $subword;
my $currentSet;
$lexicon{"\$"} = 0;				# end of utterance symbol added to lexicon with count 0
$phonemeCounts{$delimiter} = 0;

# Handle arguments
getopts('vnpw:f:');

#	open(CORPUS,"cat $corpus | tr 'A-Z' 'a-z' | tr '\-' ' ' | tr -cd '.a-z\n '|");	

# Check for feature chart file
if ($opt_f)
{
	$featureChart = FeatureChart->new();
	$featureChart->read_file($opt_f);
	# consider adding code to automatically add word boundary symbol to feature chart with unique feature... currently done by hand
	$window = 2;
#	print $featureChart;
}

if ($opt_w > 1)
{
	$window = $opt_w;
}

while (<>)
{
	chomp;
	my $sentence = $_;
	@segmentation = ();
	$sentence =~ s/((\s)|(\.))+//g;
	$segmentedSentence = $sentence;

	@bestProduct = ();
	@bestStart = ();
	
	
	for (my $lastChar = 0; $lastChar < length($sentence); $lastChar++)
	{
		push(@bestProduct, R(substr($sentence,0,$lastChar + 1)));	
		push(@bestStart, 0);
		for ($firstChar = 1; $firstChar <= $lastChar; $firstChar++)
		{
			$wordScore = R(substr($sentence,$firstChar,($lastChar + 1) - $firstChar));		
			$scoreProduct = $wordScore * $bestProduct[$firstChar - 1];
			if ($scoreProduct > $bestProduct[$lastChar])
			{
				$bestProduct[$lastChar] = $scoreProduct;
				$bestStart[$lastChar] = $firstChar;
			}
		}
	}
	if ($opt_v)
	{
		print "Best start: @bestStart\n";
		print "Best product: @bestProduct\n"		
	}
	$firstChar = $bestStart[length($sentence) - 1];
	while ($firstChar > 0)
	{
		push(@segmentation,$firstChar);
		$firstChar = $bestStart[$firstChar - 1];
	}
	@segmentation = sort { $a <=> $b } @segmentation;
	unshift(@segmentation, 0);
	push(@segmentation,length($sentence));
	my $word;
	my $phoneme;
	my $subword;
	if ($opt_v)
	{
		print "\nSegmented utterance: ";		
	}
	if ($opt_n)
	{
		print $lexicon{"\$"} + 1 . ": ";
	}
	for (my $i = 0; $i < scalar(@segmentation) - 1; $i++)
	{
		$totalWords++;
		$word = substr($sentence, $segmentation[$i], $segmentation[$i+1] - $segmentation[$i]);
		print $word . $delimiter;
		$phonemeCounts{$delimiter} += 1; 	
		$totalPhonemes += 1;
		if (exists $lexicon{$word})
		{
			$lexicon{$word} += 1;
		}			
		else
		{
			$lexicon{$word} = 1;
		}
		# NOT SURE IF THIS IS NECESSARY.  ADDED POST-NGRAM STUFF
		if ($window > 1)
		{
			$word = $delimiter . $word . $delimiter;
		} 
		else # This didn't happen before, but it seems like it should
		{
			$word = $word . $delimiter;
		}		
		if (!$opt_f)
		{			
			for (my $i = 0; $i < length($word) - ($window - 1); $i++)
			{
				$phoneme = substr($word,$i,$window);
				if (exists $phonemeCounts{$phoneme})
				{
					$phonemeCounts{$phoneme} += 1; 	
				}
				else
				{
					$phonemeCounts{$phoneme} = 1;					
				}
				$totalPhonemes += 1;
			}			
		}
		else
		{
			@phoneFeatures = ();		
			# Get all feature bundles for current word	
			for (my $i = 0; $i < length($word); $i++)
			{
				$phoneme = substr($word,$i,1);
				push(@phoneFeatures, $featureChart->featuresForPhone($phoneme))
			}			
			for (my $i = 0; $i < length($word) - ($window - 1); $i++)
			{
				@subList = @phoneFeatures[$i..$i + $window - 1];
				$currentSet = $subList[0];
				for (my $j = 1; $j < $window; $j++)
				{
					$subword = substr($word,$i,$j+1);
					if (exists $productCache{$subword})
					{
						$currentSet = $productCache{$subword};
					}
					else
					{
						$currentSet = $currentSet->cartesian_product($subList[$j]);
						@concatenatedResults = map {join "#", @{$_}} $currentSet->members;
						# print "\n\n";
						$currentSet->clear;
						# map {print "$_ ";} @concatenatedResults;
						$currentSet->insert(@concatenatedResults);
						$productCache{$subword} = $currentSet;
					}
				}
				while (defined(my $featureGram = $currentSet->each))
				{
					if (exists $phonemeCounts{$featureGram})
					{
						$phonemeCounts{$featureGram} += 1; 	
					}
					else
					{
						$phonemeCounts{$featureGram} = 1;					
					}
					$totalPhonemes += 1;
				}					
			}
		}
	}
	$totalWords++;
	$lexicon{"\$"} += 1;
	print "\$\n\n";
#	if ($lexicon{"\$"} > 105)
#	{
#		last;
#	}
}
close(CORPUS);

sub R
{
	# wordTypes is only used for novel words so the new word cancels out the subtraction
	%wordPhonemeCounts = ();
	$wordTotalPhonemes = 0;
	my $wordTypes = scalar(keys %lexicon);
	my $score = 0;
	my $phonemeScore;
	my $word = @_[0];
	my $temp;

	if ($opt_v)
	{
		print "\nConsidering: $word\n";
	}

	# familiar word
	if (exists $lexicon{$word})
	{
		$score = (($lexicon{$word} + 1) / ($totalWords + 1)) * ((($lexicon{$word}) / ($lexicon{$word} + 1)) ** 2);
	}
	# novel word
	else
	{
		# get adjusted phoneme counts
		my $wordWithBoundary;
		if ($window > 1)
		{
			$wordWithBoundary = $delimiter . $word . $delimiter;
		} 
		else
		{
			$wordWithBoundary = $word . $delimiter;
		}
		my $phoneme;
		if (!$opt_f)
		{			
			for (my $i = 0; $i < length($wordWithBoundary) - ($window - 1); $i++)
			{
				$phoneme = substr($wordWithBoundary,$i,$window);
				if (exists $wordPhonemeCounts{$phoneme})
				{
					$wordPhonemeCounts{$phoneme} += 1;
				}
				elsif (exists $phonemeCounts{$phoneme})
				{
					$wordPhonemeCounts{$phoneme} = $phonemeCounts{$phoneme} + 1;
				}
				else
				{
					$wordPhonemeCounts{$phoneme} = 1;
				}				
			}
			$wordTotalPhonemes = $totalPhonemes + (length($wordWithBoundary) - ($window - 1));			
		}
		else
		{
			$wordTotalPhonemes = $totalPhonemes;
			@phoneFeatures = ();		
			# Get all feature bundles for current word	
			for (my $i = 0; $i < length($wordWithBoundary); $i++)
			{
				$phoneme = substr($wordWithBoundary,$i,1);
				push(@phoneFeatures, $featureChart->featuresForPhone($phoneme))
			}			
			for (my $i = 0; $i < length($wordWithBoundary) - ($window - 1); $i++)
			{
				@subList = @phoneFeatures[$i..$i + $window - 1];
				$currentSet = $subList[0];
				for (my $j = 1; $j < $window; $j++)
				{
					$subword = substr($word,$i,$j+1);
					if (exists $productCache{$subword})
					{
						$currentSet = $productCache{$subword};
					}
					else
					{
						$currentSet = $currentSet->cartesian_product($subList[$j]);
						@concatenatedResults = map {join "#", @{$_}} $currentSet->members;
						# print "\n\n";
						$currentSet->clear;
						# map {print "$_ ";} @concatenatedResults;
						$currentSet->insert(@concatenatedResults);
						$productCache{$subword} = $currentSet;
					}
				}
				while (defined(my $featureGram = $currentSet->each))
				{
					if (exists $wordPhonemeCounts{$featureGram})
					{
						$wordPhonemeCounts{$featureGram} += 1; 	
					}
					else
					{
						$wordPhonemeCounts{$featureGram} = 1;					
					}
					$wordTotalPhonemes++;
				}					
			}
		}
		if ($opt_v)
		{
			print "First term: " . 6 / (pi**2) . "\n";			
			print "Second term: " . ($wordTypes / ($totalWords + 1)) . "\n";
		}
		$score = 6 / (pi**2);
		$score *= ($wordTypes / ($totalWords + 1));
		$phonemeScore = 0;
		foreach my $key (keys %lexicon)
		{
			if (!($key eq "\$"))
			{
				$phonemeScore += ProbPhonemes($key);				
			}
		}
		if ($phonemeScore > 0)
		{
			$temp = ProbPhonemes($word);
			$score *= $temp / (1 - (($wordTypes - 1) / $wordTypes) * ($temp + $phonemeScore));
			if ($opt_v)
			{				
				print "Third-top: " . $temp . "\n";
				print "Third-bottom: " . (1 - (($wordTypes - 1) / $wordTypes) * ($temp + $phonemeScore)) . "\n";
				print "Third term: " . $temp / (1 - (($wordTypes - 1) / $wordTypes) * ($temp + $phonemeScore)) . "\n";
			}
		}
		else
		{
			$score = 0;
		}
		$score *= (($wordTypes - 1) / $wordTypes) ** 2;
		if ($opt_v)
		{
			print "Fourth term: " . (($wordTypes - 1) / $wordTypes) ** 2 . "\n";			
		}
	}
	if ($opt_v)
	{
		print "Lexicon:\n@{[ %lexicon ]}\n";
		print "Actual phoneme Counts:\n@{[ %phonemeCounts ]}\n";
		print "Novel-word phoneme Counts:\n@{[ %wordPhonemeCounts ]}\n";
		print "Total phonemes: $wordTotalPhonemes\n"; 
		print "score for $word: $score\n";		
	}
	return $score;
}

sub ProbPhonemes
{
	my $word;
	my $phonemeScore;
	my $phoneme;
	my $currentSet;
	my @subList;
	my $subword;
	my @concatenatedResults;
	if ($window > 1)
	{
		$phonemeScore = 1;
		$word = $delimiter . @_[0] . $delimiter;		
	}
	else
	{
		$phonemeScore = (1 / (1 - ($wordPhonemeCounts{$delimiter}/ $wordTotalPhonemes)));
		$word = @_[0] . $delimiter;		
	}
	if (!$opt_f)
	{
		for (my $i = 0; $i < length($word) - ($window - 1); $i++)
		{
			$phoneme = substr($word,$i,$window);
			if (exists $wordPhonemeCounts{$phoneme})
			{
				$phonemeScore *= $wordPhonemeCounts{$phoneme} / $wordTotalPhonemes;
			}
			else
			{
				$phonemeScore *= $phonemeCounts{$phoneme} / $wordTotalPhonemes;
			}
		}		
	}
	else
	{
		@phoneFeatures = ();		
		# Get all feature bundles for current word	
		for (my $i = 0; $i < length($word); $i++)
		{
			$phoneme = substr($word,$i,1);
			push(@phoneFeatures, $featureChart->featuresForPhone($phoneme))
		}			
		for (my $i = 0; $i < length($word) - ($window - 1); $i++)
		{
			@subList = @phoneFeatures[$i..$i + $window - 1];
			$currentSet = $subList[0];
			for (my $j = 1; $j < $window; $j++)
			{
				$subword = substr($word,$i,$j+1);
				if (exists $productCache{$subword})
				{
					$currentSet = $productCache{$subword};
				}
				else
				{
					$currentSet = $currentSet->cartesian_product($subList[$j]);
					@concatenatedResults = map {join "#", @{$_}} $currentSet->members;
					# print "\n\n";
					$currentSet->clear;
					# map {print "$_ ";} @concatenatedResults;
					$currentSet->insert(@concatenatedResults);
					$productCache{$subword} = $currentSet;
				}
			}
			while (defined(my $featureGram = $currentSet->each))
			{
				if (exists $wordPhonemeCounts{$featureGram})
				{
					$phonemeScore *= $wordPhonemeCounts{$featureGram} / $wordTotalPhonemes;
				}
				else
				{
					$phonemeScore *= $phonemeCounts{$featureGram} / $wordTotalPhonemes;
				}
			}					
		}
	}
	return $phonemeScore;
}