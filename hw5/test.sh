#!/bin/bash

echo "=====CLEAN====="
make clean
echo ""

echo "======MAKE======"
make
echo ""

echo "================"
echo "=====RESULT====="
echo "================"

echo ""
echo "Above one is Sm5 result"
echo "Below one is K-- result"
echo ""

kmmfiles=`ls examples/*.k--`
for entry in $kmmfiles
do
  echo $entry
  sm5=`echo 10 | ./run $entry`
  kmm=`echo 10 | ./run -k $entry`
  echo $sm5
  echo $kmm
done

echo ""
echo ""
echo "================"
echo "=======GC======="
echo "================"
gc=`./gcrun`
echo "137 true true true 630 500 1234 1234"
echo $gc
