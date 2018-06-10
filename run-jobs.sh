#!/bin/bash

for model in glm rpart xgbTree svmRadial
do
  METHOD=$model bash -c "cat home-credit-default-risk.R | R --slave >log-${model}.txt"
done
