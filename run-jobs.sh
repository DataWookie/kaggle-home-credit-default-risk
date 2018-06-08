#!/bin/bash

for model in glm rpart xgboost svm
do
  cat 0-load-data.R  1-prepare.R  2-train-${model}.R  3-submit.R | R --slave >log-${model}.txt &
done
