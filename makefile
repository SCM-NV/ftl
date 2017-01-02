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
BUILD    ?= debug
BUILDDIR = build.$(PLATFORM).$(BUILD)

ifeq ($(PLATFORM), gnu)
	COMPILER = gfortran
	FLAGS = -std=f2003 -ffree-line-length-none -Wall -Wextra -Wpedantic -Wno-target-lifetime -Wno-surprising -Wno-compare-reals -J$(BUILDDIR)
	CXXCOMPILER = g++
	CXXFLAGS = -std=c++11 -Ofast -march=native
else ifeq ($(PLATFORM), intel)
	COMPILER = ifort
	FLAGS = -stand f03 -warn -module $(BUILDDIR)
	CXXCOMPILER = icpc
	CXXFLAGS = -std=c++11 -fast -xHost
else
  $(error unrecognized PLATFORM)
endif

INCLUDES = -Isrc -Itests

ifeq ($(PLATFORM)$(BUILD), gnudebug)
	FLAGS += -g -O0 -fcheck=bounds,do,mem,pointer,recursion
else ifeq ($(PLATFORM)$(BUILD), inteldebug)
	FLAGS += -g -O0 -check all -debug all -traceback
else ifeq ($(PLATFORM)$(BUILD), gnurelease)
	FLAGS += -Ofast -march=native -flto
else ifeq ($(PLATFORM)$(BUILD), intelrelease)
	FLAGS += -fast -xHost
else
  $(error unrecognized BUILD)
endif

# option to disable the use of finalizers (in case your compiler can't handle them ...)
ifeq ($(FINALIZERS), skip)
	FLAGS += -DFTL_NO_FINALIZERS
endif


# Make commands:

test: $(BUILDDIR)/tests
	./$(BUILDDIR)/tests

memcheck: $(BUILDDIR)/tests
	valgrind --leak-check=yes ./$(BUILDDIR)/tests

perftest: $(BUILDDIR)/perftest_sortVectorInt $(BUILDDIR)/perftest_sortVectorInt_ref
	./$(BUILDDIR)/perftest_sortVectorInt
	./$(BUILDDIR)/perftest_sortVectorInt_ref

$(BUILDDIR):
	mkdir $(BUILDDIR)

clean:
	rm -rf $(BUILDDIR)

cleanall:
	rm -rf build.*


# Unit tests:

$(BUILDDIR)/tests: tests/tests.F90 $(BUILDDIR)/ftlTestTools.o $(BUILDDIR)/ftlVectorTests.o $(BUILDDIR)/ftlListTests.o $(BUILDDIR)/ftlUnorderedMapTests.o $(BUILDDIR)/ftlAlgorithmsTests.o $(BUILDDIR)/ftlMemoryTests.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $< $(BUILDDIR)/*.o -o $@

$(BUILDDIR)/ftlTestTools.o: tests/ftlTestTools.F90 tests/ftlTestTools.inc | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlVectorTests.o: tests/ftlVectorTests.F90 $(BUILDDIR)/ftlVectorInt.o $(BUILDDIR)/ftlVectorPoint2D.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlListTests.o: tests/ftlListTests.F90 $(BUILDDIR)/ftlListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlUnorderedMapTests.o: tests/ftlUnorderedMapTests.F90 $(BUILDDIR)/ftlUnorderedMapStrInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlAlgorithmsTests.o: tests/ftlAlgorithmsTests.F90 $(BUILDDIR)/ftlVectorIntAlgorithms.o $(BUILDDIR)/ftlVectorPoint2DAlgorithms.o $(BUILDDIR)/ftlListIntAlgorithms.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlMemoryTests.o: tests/ftlMemoryTests.F90 $(BUILDDIR)/ftlMemoryInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@


# Individual performance tests:

$(BUILDDIR)/perftest_sortVectorInt: perftests/sortVectorInt.F90 $(BUILDDIR)/ftlTestTools.o $(BUILDDIR)/ftlVectorIntAlgorithms.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $< $(BUILDDIR)/*.o -o $@

$(BUILDDIR)/perftest_sortVectorInt_ref: perftests/sortVectorInt.cpp | $(BUILDDIR)
	$(CXXCOMPILER) $(CXXFLAGS) $< -o $@


# Container instantiations:

$(BUILDDIR)/ftlVectorInt.o: instantiations/ftlVectorInt.F90 src/ftlVector.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlVectorPoint2D.o: instantiations/ftlVectorPoint2D.F90 src/ftlVector.F90_template $(BUILDDIR)/Point2D.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlListInt.o: instantiations/ftlListInt.F90 src/ftlList.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlUnorderedMapStrInt.o: instantiations/ftlUnorderedMapStrInt.F90 src/ftlUnorderedMap.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@


# ftlAlgorithms instantiations:

$(BUILDDIR)/ftlVectorIntAlgorithms.o: instantiations/ftlVectorIntAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlVectorInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlVectorPoint2DAlgorithms.o: instantiations/ftlVectorPoint2DAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlVectorPoint2D.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlListIntAlgorithms.o: instantiations/ftlListIntAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@


# ftlMemory instantiations:

$(BUILDDIR)/ftlMemoryInt.o: instantiations/ftlMemoryInt.F90 src/ftlMemory.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@


# Non-template FTL modules:

$(BUILDDIR)/ftlHash.o: src/ftlHash.F90 | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@


# Example derived types:

$(BUILDDIR)/Point2D.o: instantiations/derived_types/Point2D.F90 | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@
