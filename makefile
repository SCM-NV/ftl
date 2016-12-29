# disable default implicit rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

PLATFORM ?= gnu
BUILD    ?= debug
BUILDDIR = build.$(PLATFORM).$(BUILD)

ifeq ($(PLATFORM), gnu)
	COMPILER = gfortran
	FLAGS    = -std=f2003 -ffree-line-length-none -Wall -Wextra -Wpedantic -Wno-target-lifetime -Wno-surprising -J$(BUILDDIR)
else ifeq ($(PLATFORM), intel)
	COMPILER = ifort
	FLAGS    = -warn -heap-arrays 1 -module $(BUILDDIR)
else
  $(error unrecognized PLATFORM)
endif

INCLUDES = -Isrc -Itests

ifeq ($(PLATFORM)$(BUILD), gnudebug)
	FLAGS += -g -O0 -fcheck=bounds,do,mem,pointer,recursion
else ifeq ($(PLATFORM)$(BUILD), inteldebug)
	FLAGS += -g -O0 -check all -debug all -traceback
else ifeq ($(PLATFORM)$(BUILD), gnurelease)
	FLAGS += -Ofast -fno-stack-arrays -march=native -flto
else ifeq ($(PLATFORM)$(BUILD), intelrelease)
	FLAGS += -fast -xHost
else
  $(error unrecognized BUILD)
endif

# option to disable the use of finalizers (in case your compiler can't handle them ...)
ifeq ($(FINALIZERS), skip)
	FLAGS += -DFTL_NO_FINALIZERS
endif

test: $(BUILDDIR)/tests
	./$(BUILDDIR)/tests

memcheck: $(BUILDDIR)/tests
	valgrind --leak-check=yes ./$(BUILDDIR)/tests

perftest: $(BUILDDIR)/perftests
	./$(BUILDDIR)/perftests

$(BUILDDIR):
	mkdir $(BUILDDIR)


$(BUILDDIR)/tests: tests/tests.F90 $(BUILDDIR)/ftlTestTools.o $(BUILDDIR)/ftlVectorTests.o $(BUILDDIR)/ftlListTests.o $(BUILDDIR)/ftlAlgorithmsTests.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $< $(BUILDDIR)/*.o -o $@

$(BUILDDIR)/ftlTestTools.o: tests/ftlTestTools.F90 tests/ftlTestTools.inc | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlVectorTests.o: tests/ftlVectorTests.F90 $(BUILDDIR)/ftlVectorInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlVectorInt.o: tests/instantiations/ftlVectorInt.F90 src/ftlVector.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlListTests.o: tests/ftlListTests.F90 $(BUILDDIR)/ftlListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlListInt.o: tests/instantiations/ftlListInt.F90 src/ftlList.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlAlgorithmsTests.o: tests/ftlAlgorithmsTests.F90 $(BUILDDIR)/ftlVectorIntAlgorithms.o $(BUILDDIR)/ftlListIntAlgorithms.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlVectorIntAlgorithms.o: tests/instantiations/ftlVectorIntAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlVectorInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlListIntAlgorithms.o: tests/instantiations/ftlListIntAlgorithms.F90 src/ftlAlgorithms.F90_template $(BUILDDIR)/ftlListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@


$(BUILDDIR)/perftests: tests/perftests.F90 $(BUILDDIR)/ftlTestTools.o $(BUILDDIR)/ftlAlgorithmsPerformanceTests.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $< $(BUILDDIR)/*.o -o $@

$(BUILDDIR)/ftlAlgorithmsPerformanceTests.o: tests/ftlAlgorithmsPerformanceTests.F90 $(BUILDDIR)/ftlVectorIntAlgorithms.o  | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@


clean:
	rm -rf $(BUILDDIR)

cleanall:
	rm -rf build.*
