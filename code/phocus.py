#!/usr/bin/env python

'''
    PHOCUS: PHOnotactic CUe Segmenter
    Copyright (C) 2007-2010 Dan Blanchard.

    This file is part of PHOCUS.

    PHOCUS is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    PHOCUS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with PHOCUS.  If not, see <http://www.gnu.org/licenses/>.
'''

import argparse
import copy
from abc import ABCMeta, abstractmethod
from fractions import Fraction
from itertools import chain, product

from probability import FreqDist, LidstoneProbDist, ConditionalFreqDist, ConditionalProbDist
from ngram import NgramModel
from nltk.util import ingrams

# Global vars
subseq_counts = FreqDist(counttype=Fraction)
args = None


class PartialCountNgramModel(NgramModel):
    def __init__(self, n, train, estimator, freqtype, *estimator_args, **estimator_kw_args):
        """
        Creates an ngram language model to capture patterns in n consecutive
        words of training text.  An estimator smooths the probabilities derived
        from the text and may allow generation of ngrams not seen during
        training.

        @param n: the order of the language model (ngram size)
        @type n: C{int}
        @param train: the training text
        @type train: C{list} of C{string} (or C{list} of C{string} C{list}s)
        @param estimator: a function for generating a probability distribution
        @type estimator: a function that takes a C{ConditionalFreqDist} and
              returns a C{ConditionalProbDist}
        @param freqtype: the type to use to store the counts in the underlying frequency distribution
        @type freqtype: any numeric type
        @param estimator_args: Extra arguments for C{estimator}.
            These arguments are usually used to specify extra
            properties for the probability distributions of individual
            conditions, such as the number of bins they contain.
        @type estimator_args: (any)
        @param estimator_kw_args: Extra keyword arguments for C{estimator}.
        @type estimator_kw_args: (any)
        """

        self._n = n

        cfd = ConditionalFreqDist(counttype=freqtype)
        self._ngrams = set()
        self._padding = ('',) * (n - 1)
        self._estimator = estimator
        self._freqtype = freqtype
        self._estimator_args = estimator_args
        self._estimator_kw_args = estimator_kw_args

        for utterance in train:
            for ngram in ingrams(chain(self._padding, utterance, self._padding), n):
                self._ngrams.add(ngram)
                context = tuple(ngram[:-1])
                token = ngram[-1]
                cfd[context].inc(token)

        self._model = ConditionalProbDist(cfd, estimator, counttype=freqtype, *estimator_args, **estimator_kw_args)

        # recursively construct the lower-order models
        if n > 1:
            self._backoff = NgramModel(n - 1, train, estimator, *estimator_args, **estimator_kw_args)

    def update(self, samples, increase_amount=1):
        cond_samples = []
        for utterance in samples:
            for ngram in ingrams(chain(self._padding, utterance, self._padding), self._n):
                self._ngrams.add(ngram)
                cond_samples.append((tuple(ngram[:-1]), ngram[-1]))
        self._model.update(cond_samples, increase_amount)

        # Recursively update lower-order models
        if self._n > 1:
            self._backoff.update(samples, increase_amount)


class Cue(object):
    """ Parent class for all PHOCUS Cues/Features """

    __metaclass__ = ABCMeta

    def __init__(self, initial_count):
        '''
            Initializes any counts to their default values, if necessary
            @param initial_count: the count to assign to unseen instances of this cues
            @type initial_count: C{Fraction}
        '''
        self._initial_count = initial_count

    @abstractmethod
    def eval_word(self, word, score_combiner):
        '''
            Returns probability that proposed word is a word.
            Takes function that determines how to combine scores in the case of sub-scores as argument.
        '''
        pass

    @abstractmethod
    def update_evidence(self, word, increase_amount):
        pass

    @abstractmethod
    def dump(self, dump_file):
        pass

    @abstractmethod
    def use_score(self, word):
        pass

    @property
    def initial_count(self):
        return self._initial_count


class FamiliarWordCue(Cue):
    '''
        Feature that scores words based on their lexical frequency.
    '''

    def __init__(self, initial_count, mbdp=False, subseq_denom=False, type_denom=False):
        super(FamiliarWordCue, self).__init__(initial_count)
        self._lexicon = FreqDist()
        self._mbdp = mbdp
        self._subseq_denom = subseq_denom
        self._type_denom = type_denom

    def in_lexicon(self, word):
        return word in self._lexicon

    @property
    def total_words(self):
        return self._lexicon.N()

    def eval_word(self, word, score_combiner):
        '''
            Returns probability that proposed word is a word.
            Takes function that determines how to combine scores in the case of sub-scores as argument.
        '''
        global subseq_counts

        if word in self._lexicon:
            word_count = Fraction(self._lexicon[word])
            if not self._mbdp:
                word_types = Fraction(self._lexicon.B() - 1)  # Subtract one for initial utterance delimiter addition
                raw_score = word_count / (Fraction(subseq_counts[word]) if self._subseq_denom
                                                                        else (Fraction(self.total_words + word_types) if self._type_denom
                                                                                                                      else Fraction(self.total_words)))
            else:
                raw_score = ((word_count + Fraction(1)) / (self.total_words + Fraction(1))) * (((word_count / (word_count + Fraction(1))) ** Fraction(2)))
        return raw_score  # lexical decay stuff would need to be added here

    def dump(self, dump_file):
        for word in self._lexicon.viewkeys():
            dump_file.write(word + str(self._lexicon[word]) + '\n')
        if self._subseq_denom:
            for seq in subseq_counts.viewkeys():
                dump_file.write(word + str(subseq_counts[seq]) + '\n')
        dump_file.close()

    def use_score(self, word):
        return (not args.noLexicon) and (self.in_lexicon(word))

    def update_evidence(self, word, increase_amount):
        self._lexicon.inc(word, increase_amount)


class NgramCue(Cue):
    '''
        Feature that scores words based on their constituent n-grams.
    '''

    def __init__(self, n, initial_count, num_unigrams, hypothetical_phonotactics=False):
        '''
            @param n: the order of the language model (ngram size)
            @type n: C{int}
            @param initial_count: the initial count used for unseen n-grams
            @type initial_count: C{Fraction}
            @param num_unigrams: the number of possible unigrams types for the current cue
            @type num_unigrams: C{int}
            @param hypothetical_phonotactics: should n-gram counts be temporarily updated to include hypothetical words' n-grams?
            @type hypothetical_phonotactics: C{bool}
        '''
        super(NgramCue, self).__init__(initial_count)

        self._n = n
        self._ngram_model = PartialCountNgramModel(n, None, LidstoneProbDist, Fraction, initial_count, bins=num_unigrams)
        self._hypothetical_phonotactics = hypothetical_phonotactics
        self._token_phonotactics = token_phonotactics

    def eval_word(self, word, score_combiner):
        '''
            Returns probability that proposed word is a word.
            Takes function that determines how to combine scores in the case of sub-scores as argument.
        '''
        global subseq_counts

        if self._hypothetical_phonotactics:
            # Make a deep copy of the ngram model so that we can update it without modifying the original
            word_ngram_model = copy.deepcopy(self._ngram_model)
            word_ngram_model.update([word])
        else:
            word_ngram_model = self._ngram_model

        # If I want to duplicate functionality of OCaml code, should probably make my own ProbDist that inherits from Lidstone,
        # otherwise this won't do the denominator adjustments and other stuff.
        return score_combiner([self._ngram_model.prob(tuple(ngram[:-1]), ngram[-1]) for ngram in
                               ingrams(chain(self._ngram_model._padding, word, self._ngram_model._padding), self._n)])

    def update_evidence(self, word, increase_amount):
        # Used to have token phonotactics check here in OCaml code, but that really breaks the separation of the different cues.
        # I'll take care of checking that the word isn't in the lexicon (or token phonotactics has been specified) outside of this class.
        self._ngram_model.update([word])

    def dump(self, dump_file):
        # TODO: implement this function
        pass

    # Might want to make this abstract and create sub-classes for phoneme and syllable n-grams
    def use_score(self, word):
        pass


if __name__ == '__main__':
    # Parse command-line arguments
    parser = argparse.ArgumentParser(description='PHOCUS is a word segmentation system.', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("-bs", "--badScore", help="Score assigned when word length is less than window size.",
                        type=Fraction, default="0.0")
    parser.add_argument("-df", "--decayFactor", help="Exponent used to calculate memory decay (0 = no decay).",
                        type=Fraction, default="0.0")
    parser.add_argument("-fc", "--featureChart", help="Feature chart file", type=argparse.FileType('r'))
    parser.add_argument("-fo", "--featureNgramsOut", help="File to dump final feature n-gram counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-fw", "--featureWindow", help="Window size for feature n-grams", type=int)
    parser.add_argument("-gp", "--goldPhonotactics",
                        help="Calculate phoneme n-gram scores based on their true frequencies in the gold corpus.",
                        action="store_true")
    parser.add_argument("-hp", "--hypotheticalPhonotactics",
                        help="When evaluating hypothetical words' well-formedness, increment counts of all n-grams within " +
                             "proposed word.",
                        action="store_true")
    parser.add_argument("-iw", "--ignoreWordBoundary",
                        help="When calculating phoneme/syllable/etc. n-gram scores, do not include word boundary.",
                        action="store_true")
    parser.add_argument("-ic", "--initialCount",
                        help="Count assigned to phonotactic largest n-grams before they are seen.",
                        type=Fraction, default="0.0001")
    parser.add_argument("-is", "--initializeSyllables",
                        help="Initialize syllable n-gram by finding all syllables in gold corpus and setting their counts to one " +
                             "in advance.",
                        action="store_true")
    parser.add_argument("-i", "--interactive",
                        help="After reading in corpus, user can specify an utterance number to segment up to, and query scores " +
                             "for possible segmentations.",
                        action="store_true")
    parser.add_argument("-jp", "--jointProbability", help="Use joint probabilities instead of conditional", action="store_true")
    parser.add_argument("-lo", "--lexiconOut", help="File to dump final lexicon to", type=argparse.FileType('w'))
    parser.add_argument("-ln", "--lineNumbers", help="Display line numbers before each segmented utterance", action="store_true")
    parser.add_argument("-mb", "--mbdp",
                        help="Use MBDP-1 (Brent 1999) phoneme and word scores functions.  Implies --initialCount = 0. Should " +
                             "also enable --hypotheticalPhonotactics for true MBDP-1.",
                        action="store_true")
    parser.add_argument("-nb", "--nBest",
                        help="Store scores for N-best segmentations instead of just the best. Should help lessen impact of " +
                             "early errors.",
                        action="store_true")
    parser.add_argument("-nl", "--noLexicon",
                        help="Only score words based on the phonotactics, and don't do 'familiar word' spotting.  Does NOT entail " +
                             "--tokenPhonotactics.",
                        action="store_true")
    parser.add_argument("-pn", "--phonemeNgramsOut", help="File to dump final phoneme n-gram counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-pc", "--piecewiseCountsOut", help="File to dump final strictly two-piecewise counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-pw", "--phonemeWindow", help="Window size for phoneme n-grams", type=int)
    parser.add_argument("-pu", "--printUtteranceDelimiter", help="Print utterance delimiter at the end of each utterance",
                        action="store_true")
    parser.add_argument("-rs", "--requireSyllabic",
                        help="Require each proposed word to contain at least one syllabic sound. " +
                             "(Requires --featureChart that includes 'syllabic' as feature)",
                        action="store_true")
    parser.add_argument("-sp", "--scorePiecewise", help="Score potential words based on their Strictly 2-Piecewise factors " +
                                                        "(i.e., long distance pairs for vowel and consonantal harmony).",
                        action="store_true")
    parser.add_argument("-st", "--stabilityThreshold",
                        help="When --waitForStablePhonemeDist is enabled, all the ratio between all phoneme counts when they are " +
                             "updated must be greater than stabilityThreshold before model will start segmenting.",
                        type=Fraction, default="0.99")
    parser.add_argument("-sd", "--subseqDenominator",
                        help="For all scores, calculate probability sequence of characters is in a word, rather than probability " +
                             "of them occuring in corpus.",
                        action="store_true")
    parser.add_argument("-sf", "--supervisedFor",
                        help="Number of utterances to use given word-boundaries for (0 = unsupervised).",
                        type=int)
    parser.add_argument("-su", "--supervisedUpdating",
                        help="When doing supervised segmenting with the supervisedFor flag, resume learning process after " +
                             "supervised portion of corpus.",
                        action="store_true")
    parser.add_argument("-so", "--syllableNgramsOut", help="File to dump final syllable n-gram counts to",
                        type=argparse.FileType('w'))
    parser.add_argument("-sw", "--syllableWindow",
                        help="Window size for syllable n-grams (Note: does not entail --initialSyllables)", type=int)
    parser.add_argument("-tp", "--tokenPhonotactics",
                        help="Update phoneme n-gram counts once per word occurrence, instead of per word type.",
                        action="store_true")
    parser.add_argument("-td", "--typeDenominator",
                        help="When evaluating familiar words, divide word count by total number of word tokens + total number of " +
                             "word types.  Should only be on for Venkataraman.",
                        action="store_true")
    parser.add_argument("-up", "--uniformPhonotactics",
                        help="Never update phonotactic n-gram counts.  Just use initial uniform distribution throughout.",
                        action="store_true")
    parser.add_argument("-ud", "--utteranceDelimiter",
                        help="Utterance delimiter (Note: Utterances are always assumed to be one-per-line, this delimiter is " +
                             "the symbol used when calculating n-grams at utterance boundaries.",
                        default="$")
    parser.add_argument("-ul", "--utteranceLimit",
                        help="Number of utterances in input corpus to process (0 = process all).", type=int,
                        default=0)
    parser.add_argument("-v", "--verbose", help="Print out scores for each possible segmentation of each utterance.",
                        action="store_true")
    parser.add_argument("-wf", "--waitForStablePhonemeDist",
                        help="Do not start attempting to segment until phoneme unigram has stabilized.", action="store_true")
    parser.add_argument("-wu", "--waitUntilUtterance",
                        help="Do not start attempting to segment until we have reached the specified utterance number.",
                        type=int)
    parser.add_argument("-ws", "--weightedSum",
                        help="Instead of using back-off model for score combination, use weighted sum with all cues " +
                             "weighted equally.",
                        action="store_true")
    parser.add_argument("-wd", "--wordDelimiter", help="Word delimiter (default: '%(default)s')", default=" ")
    args = parser.parse_args()

    if args.mbdp:
        args.initialCount = Fraction(0)
