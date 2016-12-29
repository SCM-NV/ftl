# disable default implicit rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

BUILD ?= gnu
BUILDDIR = build.$(BUILD)
ifeq ($(BUILD), gnu)
	COMPILER = gfortran
	FLAGS    = -std=f2003 -ffree-line-length-none -fcheck=bounds,do,mem,pointer,recursion -Wall -Wextra -Wpedantic -Wno-target-lifetime -Wno-surprising -g -J$(BUILDDIR)
else ifeq ($(BUILD), intel)
	COMPILER = ifort
	FLAGS    = -g -O0 -check all -debug all -warn -traceback -module $(BUILDDIR)
else
  $(error unrecognized BUILD)
endif
INCLUDES = -Isrc -Itests

# option to disable the use of finalizers (in case your compiler can't handle them ...)
ifeq ($(FINALIZERS), skip)
FLAGS += -DFTL_NO_FINALIZERS
endif


test: $(BUILDDIR)/tests
	./$(BUILDDIR)/tests

memcheck: $(BUILDDIR)/tests
	valgrind --leak-check=yes ./$(BUILDDIR)/tests

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

$(BUILDDIR)/ftlAlgorithmsTests.o: tests/ftlAlgorithmsTests.F90 $(BUILDDIR)/ftlVectorIntAlgorithms.o $(BUILDDIR)/ftlVectorInt.o $(BUILDDIR)/ftlListIntAlgorithms.o $(BUILDDIR)/ftlListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlVectorIntAlgorithms.o: tests/instantiations/ftlVectorIntAlgorithms.F90 src/ftlAlgorithms.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlListIntAlgorithms.o: tests/instantiations/ftlListIntAlgorithms.F90 src/ftlAlgorithms.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -rf $(BUILDDIR)

cleanall:
	rm -rf build.gnu build.intel
