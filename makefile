# disable default implicit rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

BUILDDIR = build
COMPILER = gfortran
FLAGS    = -std=f2003 -Wall -Wextra -Wpedantic -Wno-target-lifetime -g -J$(BUILDDIR)
INCLUDES = -Isrc

tests: $(BUILDDIR)/tests
	./$(BUILDDIR)/$@

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BUILDDIR)/tests: tests/tests.F90 $(BUILDDIR)/stdVectorInt.o $(BUILDDIR)/stdVectorReal.o | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) $^ -o $@

$(BUILDDIR)/stdVectorInt.o: tests/instantiations/stdVectorInt.F90 src/stdVector.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

$(BUILDDIR)/stdVectorReal.o: tests/instantiations/stdVectorReal.F90 src/stdVector.F90_template | $(BUILDDIR)
	$(COMPILER) $(FLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -r $(BUILDDIR)
