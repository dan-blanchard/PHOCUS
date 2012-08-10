'''
Natural Language Toolkit: Phonological Feature Chart Reader

Copyright (C) 2001-2012 NLTK Project
Author: Dan Blanchard <dblanchard@ets.org>
URL: <http://www.nltk.org/>
For license information, see LICENSE.txt
'''

from __future__ import with_statement

from collections import defaultdict
from itertools import izip

from nltk.corpus.reader import CorpusReader


class PhonologicalFeatureChartReader(CorpusReader):
    """
    Reader for tab-delimited phonological feature charts like:

    \tsyllabic\tconsonantal\tsonorant\tcontinuant\tdelayedrelease\tapproximant\tnasal\tvoice\tspreadgl\tLABIAL\tround\tlabiodental\tCORONAL\tanterior\tdistributed\tstrident\tlateral\tDORSAL
    d\t-\t+\t-\t-\t-\t-\t-\t+\t-\t-\t-\t-\t+\t+\t-\t-\t-\t-
    b\t-\t+\t-\t-\t-\t-\t-\t+\t-\t+\t-\t-\t-\t0\t0\t0\t-\t-
    ...

    Where the allowable values for every feature are +, -, and 0.
    """

    def __init__(self, root, fileids, encoding=None, delimiter='\t'):
        '''
        Initialize the feature chart.

        :param root: root directory containing feature chart files
        :type root: PathPointer or str
        :param fileids: A list of the files that make up this corpus.
            This list can either be specified explicitly, as a list of
            strings; or implicitly, as a regular expression over file
            paths.  The absolute path for each file will be constructed
            by joining the reader's root to each file name.
        :param encoding: The default unicode encoding for the files
            that make up the corpus.  The value of ``encoding`` can be any
            of the following:
            - A string: ``encoding`` is the encoding name for all files.
            - A dictionary: ``encoding[file_id]`` is the encoding
              name for the file whose identifier is ``file_id``.  If
              ``file_id`` is not in ``encoding``, then the file
              contents will be processed using non-unicode byte strings.
            - A list: ``encoding`` should be a list of ``(regexp, encoding)``
              tuples.  The encoding for a file whose identifier is ``file_id``
              will be the ``encoding`` value for the first tuple whose
              ``regexp`` matches the ``file_id``.  If no tuple's ``regexp``
              matches the ``file_id``, the file contents will be processed
              using non-unicode byte strings.
            - None: the file contents of all files will be
              processed using non-unicode byte strings.
        :param delimiter: The delimiter between the columns.
        :type delimiter: str
        '''

        super(PhonologicalFeatureChartReader, self).__init__(root, fileids, encoding)
        self._phones_to_features = defaultdict(set)
        self._features_to_phones = defaultdict(set)
        self._delimiter = delimiter
        self._feature_list = []
        self._phone_list = []
        for fileid in self.fileids():
            with self.open(fileid) as chart_file:
                first = True
                for line in chart_file:
                    line = line.rstrip()
                    split_line = line.split(self._delimiter)
                    # If this is the header line, populate the feature list
                    if first:
                        self._feature_list = split_line[1:]
                        first = False
                    # Otherwise, it's a line with feature values
                    else:
                        phone = split_line.pop(0)
                        self._phone_list.append(phone)
                        for feature, value in izip(self._feature_list, split_line):
                            if value != '0':
                                feat_val = value + feature
                                self._phones_to_features[phone].add(feat_val)
                                self._features_to_phones[feat_val].add(phone)

    @property
    def phones_to_features(self):
        '''
        Dictionary with mapping from phones to features.
        '''
        return self._phones_to_features

    def features_for_phone(self, phone):
        '''
        Get the features for the specified phone.
        '''
        return self._phones_to_features[phone]

    def phones_for_features(self, feature_values):
        '''
        Find the phones that have the specified feature values.

        :param features: The feature values to search for.
        :type features: list of strings of the form '+FEATURE' or '-FEATURE'
        :return: phones that have those features, or None if none match
        :rtype: set of str
        '''
        # If we were given a single feature as a string,fix that.
        if isinstance(feature_values, basestring):
            feature_values = [feature_values]
        return set.intersection(*[self._features_to_phones[feat_val]
                                  for feat_val in feature_values])

    def __str__(self):
        string_rep = self._delimiter + self._delimiter.join(self._feature_list) + '\n'
        for phone in self._phone_list:
            value_dict = defaultdict(int)
            for feat_val in self.features_for_phone(phone):
                value_dict[feat_val[1:]] = feat_val[0]
            string_rep += self._delimiter.join([phone] + [str(value_dict[feature]) for feature in self._feature_list]) + '\n'
        return string_rep


######################################################################
# Demo
######################################################################

def demo():
    feat_chart = PhonologicalFeatureChartReader('../corpora/', ['br-features-all.tab'])
    print "Feature Chart:"
    print feat_chart
    print "Features for 'a': {}".format(feat_chart.features_for_phone('a'))
    print "Phones that are +syllabic and -labiodental: {}".format(feat_chart.phones_for_features(['+syllabic', '-labiodental']))
    print "Phones that are +syllabic, -labiodental, and +CORONAL: {}".format(feat_chart.phones_for_features(['+syllabic', '-labiodental', '+CORONAL']))
    print

if __name__ == '__main__':
    demo()
