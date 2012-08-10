#!/usr/bin/env bash

./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sd -pw 1 -sf' ../corpora/hungarian-gold.txt > hungarian-pw1-sd-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sd -pw 2 -sf' ../corpora/hungarian-gold.txt > hungarian-pw2-sd-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sd -pw 3 -sf' ../corpora/hungarian-gold.txt > hungarian-pw3-sd-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sd -nl -tp -pw 1 -sf' ../corpora/hungarian-gold.txt > hungarian-pw1-sd-nl-tp-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sd -nl -tp -pw 2 -sf' ../corpora/hungarian-gold.txt > hungarian-pw2-sd-nl-tp-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sd -nl -tp -pw 3 -sf' ../corpora/hungarian-gold.txt > hungarian-pw3-sd-nl-tp-10fold.txt &
sleep 2
# 
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sp -sd -pw 1 -sf' ../corpora/hungarian-gold.txt > hungarian-pw1-sp-sd-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sp -sd -pw 2 -sf' ../corpora/hungarian-gold.txt > hungarian-pw2-sp-sd-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sp -sd -pw 3 -sf' ../corpora/hungarian-gold.txt > hungarian-pw3-sp-sd-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sp -sd -nl -tp -pw 1 -sf' ../corpora/hungarian-gold.txt > hungarian-pw1-sp-sd-nl-tp-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sp -sd -nl -tp -pw 2 -sf' ../corpora/hungarian-gold.txt > hungarian-pw2-sp-sd-nl-tp-10fold.txt &
sleep 2
./crossvalidate.pl -e '-s -e' -t 1 './segment.native -sp -sd -nl -tp -pw 3 -sf' ../corpora/hungarian-gold.txt > hungarian-pw3-sp-sd-nl-tp-10fold.txt &
sleep 2
# 
./segment.native -sd -pw 1 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw1-sd-sf100.txt &
sleep 2
./segment.native -sd -pw 2 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw2-sd-sf100.txt &
sleep 2
./segment.native -sd -pw 3 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw3-sd-sf100.txt &
sleep 2
./segment.native -sd -nl -tp -pw 1 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw1-sd-nl-tp-sf100.txt &
sleep 2
./segment.native -sd -nl -tp -pw 2 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw2-sd-nl-tp-sf100.txt &
sleep 2
./segment.native -sd -nl -tp -pw 3 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw3-sd-nl-tp-sf100.txt &
sleep 2

./segment.native -sp -sd -pw 1 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw1-sp-sd-sf100.txt &
sleep 2
./segment.native -sp -sd -pw 2 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw2-sp-sd-sf100.txt &
sleep 2
./segment.native -sp -sd -pw 3 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw3-sp-sd-sf100.txt &
sleep 2
./segment.native -sp -sd -nl -tp -pw 1 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw1-sp-sd-nl-tp-sf100.txt &
sleep 2
./segment.native -sp -sd -nl -tp -pw 2 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw2-sp-sd-nl-tp-sf100.txt &
sleep 2
./segment.native -sp -sd -nl -tp -pw 3 -sf 100 ../corpora/hungarian-gold.txt > hungarian-pw3-sp-sd-nl-tp-sf100.txt &
sleep 2

./segment.native -sd -pw 1 ../corpora/hungarian-gold.txt > hungarian-pw1-sd-unsupervised.txt &
sleep 2
./segment.native -sd -pw 2 ../corpora/hungarian-gold.txt > hungarian-pw2-sd-unsupervised.txt &
sleep 2
./segment.native -sd -pw 3 ../corpora/hungarian-gold.txt > hungarian-pw3-sd-unsupervised.txt &
sleep 2
./segment.native -sd -nl -tp -pw 1 ../corpora/hungarian-gold.txt > hungarian-pw1-sd-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -nl -tp -pw 2 ../corpora/hungarian-gold.txt > hungarian-pw2-sd-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sd -nl -tp -pw 3 ../corpora/hungarian-gold.txt > hungarian-pw3-sd-nl-tp-unsupervised.txt &
sleep 2

./segment.native -sp -sd -pw 1 ../corpora/hungarian-gold.txt > hungarian-pw1-sp-sd-unsupervised.txt &
sleep 2
./segment.native -sp -sd -pw 2 ../corpora/hungarian-gold.txt > hungarian-pw2-sp-sd-unsupervised.txt &
sleep 2
./segment.native -sp -sd -pw 3 ../corpora/hungarian-gold.txt > hungarian-pw3-sp-sd-unsupervised.txt &
sleep 2
./segment.native -sp -sd -nl -tp -pw 1 ../corpora/hungarian-gold.txt > hungarian-pw1-sp-sd-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sp -sd -nl -tp -pw 2 ../corpora/hungarian-gold.txt > hungarian-pw2-sp-sd-nl-tp-unsupervised.txt &
sleep 2
./segment.native -sp -sd -nl -tp -pw 3 ../corpora/hungarian-gold.txt > hungarian-pw3-sp-sd-nl-tp-unsupervised.txt &
sleep 2
