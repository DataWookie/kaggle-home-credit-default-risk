#!/bin/bash

# Requires you to have the Kaggle CLI installed.
#
# $ pip install kaggle-cli

mkdir -p data
cd data

kg download -u $KAGGLE_USER -p $KAGGLE_PASS -c home-credit-default-risk

for f in *.zip; do unzip $f; done

rm -f *.zip

