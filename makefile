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


# disable default implicit rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

PLATFORM ?= gnu
BUILD ?= debug
BUILDDIR = build.$(PLATFORM).$(BUILD)
PREFIX ?= /usr/local

INCLUDES = -Isrc -Itests
DEFINES =

ifeq ($(PLATFORM), gnu)
	COMPILER = gfortran
	FLAGS = -std=f2008 -fall-intrinsics -ffree-line-length-none -Wall -Wextra -Wpedantic -Wno-target-lifetime -Wno-compare-reals -J$(BUILDDIR)
	CXXCOMPILER = g++
	CXXFLAGS = -std=c++11 -Ofast -march=native
	SUPPRESSIONS = --suppressions=gfortran.supp
else ifeq ($(PLATFORM), intel)
	COMPILER = ifort
	FLAGS = -stand f08 -warn -diag-disable=5268 -module $(BUILDDIR)
	CXXCOMPILER = g++
	CXXFLAGS = -std=c++11 -fast -xHost
	SUPPRESSIONS =
else
  $(error unrecognized PLATFORM)
endif

USE_PCRE ?= true
ifeq ($(USE_PCRE), true)
	DEFINES += -DUSE_PCRE
	LDFLAGS = -lpcreposix -lpcre
endif

ifeq ($(PLATFORM)$(BUILD), gnudebug)
	FLAGS += -g -Og -fcheck=bounds,do,mem,pointer,recursion
else ifeq ($(PLATFORM)$(BUILD), inteldebug)
	FLAGS += -g -O0 -check all -debug all -traceback
else ifeq ($(PLATFORM)$(BUILD), gnurelease)
	FLAGS += -O2 -march=native -flto
else ifeq ($(PLATFORM)$(BUILD), intelrelease)
	FLAGS += -O3 -ipo -xHost
else
  $(error unrecognized BUILD)
endif


# Make commands:

libftl: $(BUILDDIR)/libftl.so

install: libftl
	mkdir -p $(PREFIX)/lib
	cp $(BUILDDIR)/libftl.so $(PREFIX)/lib/
	mkdir -p $(PREFIX)/include/ftl
	cp $(BUILDDIR)/ftlhashmodule.mod $(PREFIX)/include/ftl
	cp $(BUILDDIR)/ftlregexmodule.mod $(PREFIX)/include/ftl
	cp $(BUILDDIR)/ftlstringmodule.mod $(PREFIX)/include/ftl
	cp src/*.F90_template $(PREFIX)/include/ftl
	cp src/ftlMacros.inc $(PREFIX)/include/ftl

test: $(BUILDDIR)/tests
	./$(BUILDDIR)/tests

memcheck: $(BUILDDIR)/tests
	valgrind --leak-check=yes $(SUPPRESSIONS) ./$(BUILDDIR)/tests

perftest: $(BUILDDIR)/perftest_sortDynArrayInt $(BUILDDIR)/perftest_sortDynArrayInt_ref $(BUILDDIR)/perftest_countDistinctWords
	./$(BUILDDIR)/perftest_countDistinctWords
	./perftests/countDistinctWords.py
	./$(BUILDDIR)/perftest_sortDynArrayInt
	./$(BUILDDIR)/perftest_sortDynArrayInt_ref

$(BUILDDIR):
	mkdir $(BUILDDIR)

clean:
	rm -rf $(BUILDDIR)

cleanall:
	rm -rf build.* src/configure_ftlRegex.inc


# Shared library of non-template components:

$(BUILDDIR)/libftl.so: $(BUILDDIR)/ftlString.o $(BUILDDIR)/ftlHash.o $(BUILDDIR)/ftlRegex.o
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) $^ $(LDFLAGS) -shared -o $@


# Unit tests:

$(BUILDDIR)/tests: tests/tests.F90 $(BUILDDIR)/ftlTestTools.o $(BUILDDIR)/ftlArrayTests.o $(BUILDDIR)/ftlDynArrayTests.o $(BUILDDIR)/ftlListTests.o $(BUILDDIR)/ftlHashMapTests.o $(BUILDDIR)/ftlAlgorithmsTests.o $(BUILDDIR)/ftlSharedPtrTests.o $(BUILDDIR)/ftlStringTests.o $(BUILDDIR)/ftlRegexTests.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) $< $(BUILDDIR)/*.o $(LDFLAGS) -o $@

$(BUILDDIR)/ftlTestTools.o: tests/ftlTestTools.F90 tests/ftlTestTools.inc | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlArrayTests.o: tests/ftlArrayTests.F90 $(BUILDDIR)/ftlArrayIntAlgorithms.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlDynArrayTests.o: tests/ftlDynArrayTests.F90 $(BUILDDIR)/ftlDynArrayInt.o $(BUILDDIR)/ftlDynArrayPoint2D.o $(BUILDDIR)/ftlDynArrayLeaky.o $(BUILDDIR)/ftlDynArrayMovableLeaky.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlListTests.o: tests/ftlListTests.F90 $(BUILDDIR)/ftlListInt.o $(BUILDDIR)/ftlListLeaky.o $(BUILDDIR)/ftlListMovableLeaky.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlHashMapTests.o: tests/ftlHashMapTests.F90 $(BUILDDIR)/ftlHashMapStrInt.o $(BUILDDIR)/ftlHashMapStringInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlAlgorithmsTests.o: tests/ftlAlgorithmsTests.F90 $(BUILDDIR)/ftlArrayIntAlgorithms.o $(BUILDDIR)/ftlDynArrayIntAlgorithms.o $(BUILDDIR)/ftlDynArrayPoint2DAlgorithms.o $(BUILDDIR)/ftlListIntAlgorithms.o $(BUILDDIR)/ftlStringAlgorithms.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlSharedPtrTests.o: tests/ftlSharedPtrTests.F90 $(BUILDDIR)/ftlSharedPtrInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlStringTests.o: tests/ftlStringTests.F90 $(BUILDDIR)/ftlString.o $(BUILDDIR)/ftlDynArrayString.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlRegexTests.o: tests/ftlRegexTests.F90 $(BUILDDIR)/ftlRegex.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@


# Individual performance tests:

$(BUILDDIR)/perftest_sortDynArrayInt: perftests/sortDynArrayInt.F90 $(BUILDDIR)/ftlTestTools.o $(BUILDDIR)/ftlDynArrayIntAlgorithms.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) $< $(BUILDDIR)/*.o $(LDFLAGS) -o $@

$(BUILDDIR)/perftest_sortDynArrayInt_ref: perftests/sortDynArrayInt.cpp | $(BUILDDIR)
	$(CXXCOMPILER) $(CXXFLAGS) $(DEFINES) $< -o $@

$(BUILDDIR)/perftest_countDistinctWords: perftests/countDistinctWords.F90 $(BUILDDIR)/ftlString.o $(BUILDDIR)/ftlHashMapStringInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) $< $(BUILDDIR)/*.o $(LDFLAGS) -o $@


# Container instantiations:

$(BUILDDIR)/ftlDynArrayInt.o: instantiations/ftlDynArrayInt.F90 src/ftlDynArray.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlDynArrayPoint2D.o: instantiations/ftlDynArrayPoint2D.F90 src/ftlDynArray.F90_template $(BUILDDIR)/Point2D.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlDynArrayLeaky.o: instantiations/ftlDynArrayLeaky.F90 src/ftlDynArray.F90_template $(BUILDDIR)/Leaky.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlDynArrayMovableLeaky.o: instantiations/ftlDynArrayMovableLeaky.F90 src/ftlDynArray.F90_template $(BUILDDIR)/Leaky.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlDynArrayString.o: src/instantiations/ftlDynArrayString.F90 src/ftlDynArray.F90_template $(BUILDDIR)/ftlString.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlListInt.o: instantiations/ftlListInt.F90 src/ftlList.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlListLeaky.o: instantiations/ftlListLeaky.F90 src/ftlList.F90_template $(BUILDDIR)/Leaky.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlListMovableLeaky.o: instantiations/ftlListMovableLeaky.F90 src/ftlList.F90_template $(BUILDDIR)/Leaky.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlHashMapStrInt.o: instantiations/ftlHashMapStrInt.F90 src/ftlHashMap.F90_template $(BUILDDIR)/ftlHash.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlHashMapStringInt.o: instantiations/ftlHashMapStringInt.F90 src/ftlHashMap.F90_template $(BUILDDIR)/ftlHash.o $(BUILDDIR)/ftlString.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@


# ftlAlgorithms instantiations:

$(BUILDDIR)/ftlArrayIntAlgorithms.o: instantiations/ftlArrayIntAlgorithms.F90 src/ftlArray.F90_template src/ftlAlgorithms.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlDynArrayIntAlgorithms.o: instantiations/ftlDynArrayIntAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlDynArrayInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlDynArrayPoint2DAlgorithms.o: instantiations/ftlDynArrayPoint2DAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlDynArrayPoint2D.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlListIntAlgorithms.o: instantiations/ftlListIntAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/ftlStringAlgorithms.o: src/instantiations/ftlStringAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlString.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@


# ftlSharedPtr instantiations:

$(BUILDDIR)/ftlSharedPtrInt.o: instantiations/ftlSharedPtrInt.F90 src/ftlSharedPtr.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@


# Non-template FTL modules:

$(BUILDDIR)/ftlHash.o: src/ftlHash.F90 | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -fPIC -c $< -o $@

$(BUILDDIR)/ftlString.o: src/ftlString.F90 $(BUILDDIR)/ftlHash.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -fPIC -c $< -o $@

$(BUILDDIR)/ftlRegex.o: src/ftlRegex.F90 src/configure_ftlRegex.inc $(BUILDDIR)/ftlString.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -fPIC -c $< -o $@

src/configure_ftlRegex.inc: configure/configure_ftlRegex.c
	$(CXXCOMPILER) $(DEFINES) configure/configure_ftlRegex.c -o configure/configure_ftlRegex
	./configure/configure_ftlRegex | tee src/configure_ftlRegex.inc
	rm configure/configure_ftlRegex


# Example derived types:

$(BUILDDIR)/Point2D.o: instantiations/derived_types/Point2D.F90 | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@

$(BUILDDIR)/Leaky.o: instantiations/derived_types/Leaky.F90 | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $(DEFINES) -c $< -o $@
