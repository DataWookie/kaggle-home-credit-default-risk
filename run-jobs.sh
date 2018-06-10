#!/bin/bash

for model in glm rpart svmRadial xgbTree
do
  DEBUG=FALSE METHOD=$model bash -c "cat home-credit-default-risk.R | R --slave >log-${model}.txt 2>&1" &
done
