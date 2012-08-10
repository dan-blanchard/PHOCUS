#!/bin/bash

# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 1 -nl -tp -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 2 -nl -tp -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 3 -nl -tp -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 1 -nl -tp -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 2 -nl -tp -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 3 -nl -tp -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 1 -sn br-sw1-sd-rs-nl-tp-10fold.ngrams -is -ic 0.0001 -pw 0 -nl -tp -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 2 -sn br-sw2-sd-rs-nl-tp-10fold.ngrams -is -ic 0.0001 -pw 0 -nl -tp -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 3 -sn br-sw3-sd-rs-nl-tp-10fold.ngrams -is -ic 0.0001 -pw 0 -nl -tp -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw1-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw2-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/br-gold.txt > br-pw3-sd-nl-tp-sf1.txt &
# sleep 2
./segment.native -sd -rs -pw 1 -nl -tp ../corpora/br-gold.txt > br-pw1-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -pw 2 -nl -tp ../corpora/br-gold.txt > br-pw2-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -pw 3 -nl -tp ../corpora/br-gold.txt > br-pw3-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp ../corpora/br-gold.txt > br-sw1-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp ../corpora/br-gold.txt > br-sw2-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp ../corpora/br-gold.txt > br-sw3-sd-rs-nl-tp-unsupervised.txt &
sleep 2


# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 1 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 2 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 3 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 1 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 2 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 3 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 1 -sn sesotho-word-sw1-sd-rs-nl-tp-10fold.ngrams -is -ic 0.0001 -pw 0 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 2 -sn sesotho-word-sw2-sd-rs-nl-tp-10fold.ngrams -is -ic 0.0001 -pw 0 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 3 -sn sesotho-word-sw3-sd-rs-nl-tp-10fold.ngrams -is -ic 0.0001 -pw 0 -nl -tp -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-10fold.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 1 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf9.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 2 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf8.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 3 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf7.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 4 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf6.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 5 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf5.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 6 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf4.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 7 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf3.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 8 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf2.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -pw 1 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -pw 2 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -pw 3 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-sf1.txt &
# sleep 2
# ./crossvalidate.pl -d -e '-s -e' -t 9 './segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp -su -sf' ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-sf1.txt &
# sleep 2
./segment.native -sd -rs -pw 1 -nl -tp ../corpora/sesotho-word-gold.txt > sesotho-word-pw1-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -pw 2 -nl -tp ../corpora/sesotho-word-gold.txt > sesotho-word-pw2-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -pw 3 -nl -tp ../corpora/sesotho-word-gold.txt > sesotho-word-pw3-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -sw 1 -is -ic 0.0001 -pw 0 -nl -tp ../corpora/sesotho-word-gold.txt > sesotho-word-sw1-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -sw 2 -is -ic 0.0001 -pw 0 -nl -tp ../corpora/sesotho-word-gold.txt > sesotho-word-sw2-sd-rs-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -rs -sw 3 -is -ic 0.0001 -pw 0 -nl -tp ../corpora/sesotho-word-gold.txt > sesotho-word-sw3-sd-rs-nl-tp-unsupervised.txt &
sleep 2
