#!/usr/bin/perl

# Dan Blanchard
# MBDP Implementation

# usage: ./brent.pl [-v] [-n] [-q] [-w WINDOW_SIZE -b MIN_WINDOW_SIZE -d NGRAMFILE] [-l LEXICON_OUT ][-f FEATURE_CHART] FILE

use Math::Trig;
use strict;
use Getopt::Std;
use FeatureChart;
use List::Util qw(first max maxstr min minstr reduce shuffle sum);
use Readonly;

# Constants
Readonly::Scalar my $delimiter => " ";			# word delimiter
Readonly::Scalar my $utteranceDelimiter => "\$";
Readonly::Scalar my $sixOverPiSquared => 6 / (pi**2);

our ($opt_v, $opt_n, $opt_w, $opt_f, $opt_b, $opt_d, $opt_l, $opt_q);
my $window = 1;
my %lexicon = ();
my %prefixes = (); 				# all prefixes and lexical items
my %phonemeCounts = ();			# This stores phoneme counts (be they phonemes, phoneme n-grams, or feature n-grams)
my $totalWords = 0;
my $totalPhonemes = 0;
my @words;
my $firstChar;
my @segmentation;
my %wordPhonemeCounts = ();		# Phoneme counts for novel word
my $wordTotalPhonemes = 0;		# Total phonemes for novel word
my $featureChart;
my @phoneFeatures;
my %productCache = ();
my $currentLine;
$lexicon{$utteranceDelimiter} = 0;				# end of utterance symbol added to lexicon with count 0
$phonemeCounts{$delimiter} = 0;

# Handle arguments
getopts('vnqw:b:d:l:f:');

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

sub processSentence
{
	my $sentence = shift;
	my $segmentedSentence = $sentence;
	$sentence =~ s/\Q$delimiter\E+//g;
	my @segmentation;
	my @bestProduct;
	my @bestStart;
	my $wordScore;
	my $scoreProduct;
	my %liveNodes = ();
	my %syncNodeCounts = ();
	my %deadForFamiliar = ();
	my %deadForNovel = ();
	my $subUtterance;
	my $syncNode;
	for (my $lastChar = 0; $lastChar < length($sentence); $lastChar++)
	{
		$subUtterance = substr($sentence,0, $lastChar + 1);		
		push(@bestProduct, R($subUtterance));
		push(@bestStart, 0);
		%liveNodes = ();
		$liveNodes{0} = 1;
		%syncNodeCounts = ();
		$syncNodeCounts{0} = 0;
		if (!(exists $prefixes{$subUtterance}))
		{
			$deadForFamiliar{0} = 1;
		}
		# After loop, bestStart[lastChar] points to beginning of the optimal word ending with lastChar
		# bestProduct[lastChar] contains the actual score for the optimal word
		for ($firstChar = 1; $firstChar <= $lastChar; $firstChar++)
		{
			$syncNodeCounts{$firstChar} = 0;
			$subUtterance = substr($sentence, $firstChar, ($lastChar + 1) - $firstChar);
			if (!(exists $deadForNovel{$firstChar}) || !(exists $deadForNovel{$firstChar}))
			{
				$liveNodes{$firstChar} = 1;
				if ($opt_q)
				{
					if (!(exists $prefixes{$subUtterance}))
					{
						$deadForFamiliar{$firstChar} = 1;
						if (exists $deadForNovel{$firstChar})
						{
							delete $liveNodes{$firstChar};
						}
					}
				}
			}
			if (exists $liveNodes{$firstChar})
			{
				$wordScore = R($subUtterance);
				$scoreProduct = $wordScore * $bestProduct[$firstChar - 1];
				if ($opt_q)
				{
					if ($scoreProduct > $bestProduct[$lastChar])
					{
						$bestProduct[$lastChar] = $scoreProduct;
						$bestStart[$lastChar] = $firstChar;
						if (exists $deadForFamiliar{$firstChar})
						{
							foreach my $liveNode (keys %liveNodes)
							{
								if (($liveNode < $firstChar) && (!(exists $lexicon{substr($sentence, $liveNode)})))
								{
									$deadForNovel{$liveNode} = 1;
									if (exists $deadForFamiliar{$liveNode})
									{
										delete $liveNodes{$liveNode};
									}
								}
							}						
						}
					}
					if ((exists $deadForNovel{$firstChar}) && (exists $deadForNovel{$firstChar}))
					{
						delete $liveNodes{$firstChar};
					}					
				}
				else
				{
					if ($scoreProduct > $bestProduct[$lastChar])
					{
						$bestProduct[$lastChar] = $scoreProduct;
						$bestStart[$lastChar] = $firstChar;
					}					
				}
			}
		}
		if ($opt_q)
		{			
			foreach my $liveNode (keys %liveNodes)
			{
				$syncNode = $bestStart[$liveNode - 1];
				while ($syncNode > 0)
				{
					$syncNodeCounts{$syncNode} = $syncNodeCounts{$syncNode} + 1;
					if ($syncNodeCounts{$syncNode} == scalar(keys %liveNodes))
					{
						$firstChar = $bestStart[$syncNode];
						while ($firstChar > 0)
						{
							push(@segmentation, $firstChar);
							$firstChar = $bestStart[$firstChar - 1];
						}
						updateLexicon(substr($sentence, 0, $syncNode), 0, @segmentation);
						return substr($sentence, $syncNode);
					}
					$syncNode = $bestStart[$syncNode - 1];
				}
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
		push(@segmentation, $firstChar);
		$firstChar = $bestStart[$firstChar - 1];
	}
	updateLexicon($sentence, 1, @segmentation);
	return "";
}

sub updateLexicon
{
	my $sentence = shift;
	my $endOfUtterance = shift;
	my @segmentation = @_;
	@segmentation = sort { $a <=> $b } @segmentation;
	unshift(@segmentation, 0);
	if ($segmentation[-1] != length($sentence))
	{
		push(@segmentation,length($sentence));				
	}
	my $word;
	my $phoneme;
	my $subword;
	my $wordWindow = $window;
	my $wordWithBoundary;
	my $currentSet;
	my @subList;
	my @concatenatedResults;
	for (my $i = 0; $i < scalar(@segmentation) - 1; $i++)
	{
		$totalWords++;
		$word = substr($sentence, $segmentation[$i], $segmentation[$i+1] - $segmentation[$i]);
		print $word . $delimiter;
		for (my $j = 1; $j <= length($word); $j++)
		{
			$subword = substr($word, 0, $j);
			if (exists $prefixes{$subword})
			{
				$prefixes{$subword} += 1;
			}
			else
			{
				$prefixes{$subword} = 1;
			}
		}
		if (exists $lexicon{$word})
		{
			$lexicon{$word} += 1;
		}
		else
		{
			$lexicon{$word} = 1;
		}
		$wordWithBoundary = $word . $delimiter;
		if ($wordWindow > 1)
		{
			$wordWithBoundary = $delimiter . $wordWithBoundary;
		}
		# Backoff for words shorter than n
		if (length($wordWithBoundary) < $window)
		{
			$wordWindow = length($wordWithBoundary);
		}
		if (!$opt_f)
		{
			for (my $i = 0; $i < length($wordWithBoundary) - ($wordWindow - 1); $i++)
			{
				$phoneme = substr($wordWithBoundary, $i, $wordWindow);
				if (exists $phonemeCounts{$phoneme})
				{
					$phonemeCounts{$phoneme} += 1;
				}
				else
				{
					$phonemeCounts{$phoneme} = 1;
				}
				$totalPhonemes++;
			}
		}
		else
		{
			@phoneFeatures = ();
			# Get all feature bundles for current word
			for (my $i = 0; $i < length($wordWithBoundary); $i++)
			{
				$phoneme = substr($wordWithBoundary, $i,1);
				push(@phoneFeatures, $featureChart->featuresForPhone($phoneme))
			}
			for (my $i = 0; $i < length($wordWithBoundary) - ($wordWindow - 1); $i++)
			{
				@subList = @phoneFeatures[$i..$i + $wordWindow - 1];
				$currentSet = $subList[0];
				for (my $j = 1; $j < $wordWindow; $j++)
				{
					$subword = substr($wordWithBoundary, $i, $j+1);
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
					$totalPhonemes++;
				}
			}
		}
	}
	if ($endOfUtterance)
	{
		$totalWords++; # not sure this extra + 1 to total words is necessary.  appears to be for utteranceDelimiter
		$lexicon{$utteranceDelimiter} += 1;
		print "$utteranceDelimiter\n";
	}
	if ($opt_v)
	{
		"\n";
	}
}

while (<>)
{
	chomp;
	$currentLine = $_;
	if ($opt_n)
	{
		print $lexicon{$utteranceDelimiter} + 1 . ": ";
	}
	if ($opt_v)
	{
		print "\nSegmented utterance: ";
	}

	while ($currentLine)
	{
		$currentLine = processSentence($currentLine);
	}

	# if ($lexicon{$utteranceDelimiter} > 2)
	# {
	# 	last;
	# }
}

# dump ngram grammar to file if specified
if ($opt_d)
{
	open(NGRAMFILE, ">$opt_d");
	foreach my $key (keys %phonemeCounts)
	{
		print NGRAMFILE $key . "\t" . $phonemeCounts{$key} . "\n";
	}
	close(NGRAMFILE);
}

# dump ngram grammar to file if specified
if ($opt_l)
{
	open(LEXICON, ">$opt_l");
	foreach my $key (keys %lexicon)
	{
		print LEXICON $key . "\t" . $lexicon{$key} . "\n";
	}
	close(LEXICON);
}

sub R
{
	# wordTypes is only used for novel words so the new word cancels out the subtraction
	%wordPhonemeCounts = ();
	$wordTotalPhonemes = $totalPhonemes;
	my $wordTypes = scalar(keys %lexicon);
	my $score = 0;
	my $phonemeScore;
	my $word = @_[0];
	my $wordWindow = $window;
	my $temp;
	my $phoneme;
	my $currentSet;
	my @subList;
	my $subword;
	my @concatenatedResults;

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
		$wordWithBoundary = $word . $delimiter;
		if ($wordWindow > 1)
		{
			$wordWithBoundary = $delimiter . $wordWithBoundary;
		}
		# If this is set to length($wordWithBoundary) instead of length($word), the model drastically over-segments
		if (length($word) < $window)
		{
			if ($opt_b && (length($wordWithBoundary) >= $opt_b))
			{
				$wordWindow = length($wordWithBoundary);
			}
			else
			{
				return 0;
			}
		}
		if (!$opt_f)
		{
			# Get adjusted phoneme counts for posited word
			for (my $i = 0; $i < length($wordWithBoundary) - ($wordWindow - 1); $i++)
			{
				$phoneme = substr($wordWithBoundary, $i, $wordWindow);
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
				$wordTotalPhonemes++;
			}
		}
		else
		{
			@phoneFeatures = ();
			# Get all feature bundles for current word
			for (my $i = 0; $i < length($wordWithBoundary); $i++)
			{
				$phoneme = substr($wordWithBoundary, $i,1);
				push(@phoneFeatures, $featureChart->featuresForPhone($phoneme))
			}
			for (my $i = 0; $i < length($wordWithBoundary) - ($wordWindow - 1); $i++)
			{
				@subList = @phoneFeatures[$i..$i + $wordWindow - 1];
				$currentSet = $subList[0];
				for (my $j = 1; $j < $wordWindow; $j++)
				{
					$subword = substr($wordWithBoundary, $i, $j+1); # Used to be $word, but I think that was wrong
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
					# Added this elsif clause because i thought it was necessary, but unsure.  look over
					elsif (exists $phonemeCounts{$featureGram})
					{
						$wordPhonemeCounts{$featureGram} = $phonemeCounts{$featureGram} + 1;
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
			print "First term: " . $sixOverPiSquared . "\n";
			print "Second term: " . ($wordTypes / ($totalWords + 1)) . "\n";
		}
		$score = $sixOverPiSquared;
		$score *= ($wordTypes / ($totalWords + 1));
		$score *= ProbPhonemes($word);
		if ($opt_v)
		{
			print "Third term: " . $temp . "\n";
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
	my $word = @_[0] . $delimiter;
	my $phonemeScore = 1;
	my $phoneme;
	my $currentSet;
	my @subList;
	my $subword;
	my @concatenatedResults;
	my $wordWindow = $window;
	if ($wordWindow > 1)
	{
		$word = $delimiter . $word;
	}
	else
	{
		$phonemeScore = (1 / (1 - ($wordPhonemeCounts{$delimiter}/ $wordTotalPhonemes)));
	}
	if (length($word) < $window)
	{
		if ($opt_b && (length($word) >= $opt_b))
		{
			$wordWindow = length($word);
		}
		else
		{
			return 0;
		}
	}
	if (!$opt_f)
	{
		for (my $i = 0; $i < length($word) - ($wordWindow - 1); $i++)
		{
			$phoneme = substr($word, $i, $wordWindow);
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
			$phoneme = substr($word, $i,1);
			push(@phoneFeatures, $featureChart->featuresForPhone($phoneme))
		}
		for (my $i = 0; $i < length($word) - ($wordWindow - 1); $i++)
		{
			@subList = @phoneFeatures[$i..$i + $wordWindow - 1];
			$currentSet = $subList[0];
			for (my $j = 1; $j < $wordWindow; $j++)
			{
				$subword = substr($word, $i, $j+1);
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
