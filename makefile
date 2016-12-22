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

$(BUILDDIR)/tests: tests/tests.F90 $(BUILDDIR)/ftlTestTools.o $(BUILDDIR)/ftlVectorTests.o $(BUILDDIR)/ftlListTests.o $(BUILDDIR)/ftlSortTests.o | $(BUILDDIR)
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

$(BUILDDIR)/ftlSortTests.o: tests/ftlSortTests.F90 $(BUILDDIR)/ftlSortftlVectorInt.o $(BUILDDIR)/ftlVectorInt.o $(BUILDDIR)/ftlSortftlListInt.o $(BUILDDIR)/ftlListInt.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlSortftlVectorInt.o: tests/instantiations/ftlSortftlVectorInt.F90 src/ftlSort.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/ftlSortftlListInt.o: tests/instantiations/ftlSortftlListInt.F90 src/ftlSort.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -r $(BUILDDIR)
