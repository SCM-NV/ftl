#!/usr/bin/env sh
gfortran -std=f2003 -Wall -Wextra -Wpedantic -Wno-target-lifetime -g stdVectorInt.F90 stdVectorReal.F90 test.F90 -o test
