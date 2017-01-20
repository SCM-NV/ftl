#!/usr/bin/env python

# Copyright (c) 2016, 2017  Robert Rüger
#
# This file is part of of the Fortran Template Library.
#
# The Fortran Template Library is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# The Fortran Template Library is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.


import sys
import string
import time

def countDistictWords(filename):

    start = time.clock()

    # Step 1: Read the entire book into a string.
    try:
        with open(filename,'r') as f:
            contents = f.read()
    except:
        print('Unable to open file.')
        sys.exit(1)

    # Step 2: Replace all that doesn't belong into a proper word with spaces and lowercase everything.
    charsToRemove = string.punctuation + string.whitespace.replace(' ', '')
    for c in charsToRemove:
        contents = contents.replace(c, ' ')
    contents = contents.lower()

    # Step 3: Split the ftlString up into an array of words.
    words = contents.split()
    print('Number of words: %i'%(len(words)))

    # Step 4: Count the number of distinct words.
    wordOcc = {}
    for word in words:
        if word in wordOcc:
            wordOcc[word] += 1
        else:
            wordOcc[word] = 1
    print('Number of distinct words: %i'%(len(wordOcc)))

    finish = time.clock()
    print('Counted all distinct words in %s in %f s'%(filename, finish-start))


countDistictWords('tests/assets/frankenstein.txt')
countDistictWords('tests/assets/mobydick.txt')
