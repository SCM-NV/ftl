# disable default implicit rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

BUILDDIR = build
COMPILER = gfortran
FLAGS    = -std=f2003 -ffree-line-length-none -Wall -Wextra -Wpedantic -Wno-target-lifetime -Wno-surprising -g -J$(BUILDDIR)
INCLUDES = -Isrc -Itests

memcheck: $(BUILDDIR)/tests
	valgrind --leak-check=yes ./$(BUILDDIR)/tests

test: $(BUILDDIR)/tests
	./$(BUILDDIR)/tests

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BUILDDIR)/tests: tests/tests.F90 $(BUILDDIR)/stdTestTools.o $(BUILDDIR)/stdVectorTests.o $(BUILDDIR)/stdListTests.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $< $(BUILDDIR)/*.o -o $@

$(BUILDDIR)/stdTestTools.o: tests/stdTestTools.F90 tests/stdTestTools.inc | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/stdVectorTests.o: tests/stdVectorTests.F90 $(BUILDDIR)/stdVectorInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/stdVectorInt.o: tests/instantiations/stdVectorInt.F90 src/stdVector.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/stdListTests.o: tests/stdListTests.F90 $(BUILDDIR)/stdListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/stdListInt.o: tests/instantiations/stdListInt.F90 src/stdList.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -r $(BUILDDIR)
