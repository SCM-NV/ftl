============================
The Fortran Template Library
============================

The Fortran Template Library (FTL) is a general purpose library for Fortran
2003. Its intention is to bring all these nice things we take for granted in
modern languages like Python and C++ to the Fortran world: Generic containers,
versatile algorithms, easy string manipulation, and more. It is heavily inspired
by C++'s standard library, especially the part that is commonly referred to as
the Standard Template Library (STL).


Introduction
############

Fortran has come pretty far with the 2003 and 2008 standards, yet one thing that
is still missing are generic programming facilities, meaning a way to write code
that works with *any* type. In C++ this is done with templates. Let's have a
look at how one would define a generic ``max()`` function.

.. code:: c++

   template <typename T>
   T max(T x, T y) {
      T value;
      if (x < y)
         value = y;
      else
         value = x;
      return value;
   }

Once this function template is defined one can just call it like any other
function.

.. code:: c++

    bigger_integer = max(1,6)
    bigger_float   = max(1.2,6.4)

The compiler will determine that the type ``T`` is ``int``  for the first call
and ``float`` for the second and will generate two versions of the max function
template: One for ``int`` and ``float``. This is called template instantiation.
This does not only work for the plain old types like numbers, but for *any*
type, provided that it overloads the ``<`` operator. If it doesn't, the compiler
can not instantiate the the template and will produce an error.

In Python we can do pretty much the same thing, only that we would get a
run-time error in the ``max()`` function if it is called with type for which the
``<`` operator is not overloaded.

Now Fortran obviously does not have something like C++'s templates or Python's
duck typing. But really what we need to do is not *that* complicated: We only
need to replace ``T`` with our desired type!

.. code:: fortran

   T function max(x, y)
      T :: x, y
      if (x < y) then
         max = y
      else
         max = x
      endif
   end function

If we put this into a separate file, say ``max.F90_template``, we can easily
instantiate our templates manually using the C preprocessor.

.. code:: fortran

   #define T integer
   #include "max.F90_template"

   #define T real
   #include "max.F90_template"

Sure, it's not as convenient as in C++ where the compiler instantiates the
templates automatically, but it's better than nothing. Now for something as
simple as a ``max()`` function this is probably not worth it. But if your
template is a dictionary class with two template types (key and value) you
*really* don't want to duplicate its implementation for all combinations of key
and value types!

In practice we would need to put our template function into a module and and
wrap in an interface so that the different instantiations don't collide with
each other, but these are technical details that are all hidden inside the
template file. From the outside we don't mind; all we have to do is instantiate
our template once.


Components
##########

ftlDynArray
   A resizeable array container. It can slowly grow in size as elements are
   added at it's end. Note that insertion at the end is an amortized constant
   time operation. Basically, ftlDynArray is *exactly* the same as C++'s
   std::vector. I just changed the name because calling a resizeable array a
   vector makes no sense from a mathematical point of view.

ftlList
   A linked list container that allows constant time insert and erase operations
   anywhere within the list. *Exactly* the same as C++'s std::list.

ftlHashMap
   An associative containers that stores elements formed by the combination of a
   key value and a mapped value, and which allows for fast retrieval of
   individual elements based on their keys. It's basically a dictionary that
   internally uses a hash table to allow constant time retrieval of elements.
   ftlHashMap is very similar to C++'s std::unordered_map (though its interface
   is a bit less awkward).

ftlHash
   A small utility library that provides hash functions for the Fortran
   intrinsic types. This allows them to be used as key types in ftlHashMap.
   Furthermore these basic has functions can be used to implement hash functions
   for other derived types, so that these can also be used as keys in
   ftlHashMap. This file is not a template.

ftlString
   A variable length string type that integrates seamlessly with plain Fortran
   strings. The provided ftlString type is not a template. It is quite similar
   to C++'s std::string in the sense that it has the interface of a container of
   single characters. However, since the std::string interface is in practice a
   bit basic, it also offers Python's string manipulation methods.

ftlAlgorithms
   A library of generic algorithms that work on all FTL containers. *Exactly* the
   same as C++'s std::algorithm header.

ftlMemory
   Provides general utilities to manage dynamic memory. At the moment only
   contains a reference counted ftlSharedPtr in the spirit of C++'s
   std::shared_ptr.


Implementation progress
#######################

ftlDynArray, ftlList and ftlHashMap are pretty much finished.

ftlAlgorithms is incomplete. Ultimately I would like all of the algorithms in
C++'s std::algorithm header to be implemented, but so far I only did maybe 30%
of them. It's quite a lot of work as there are many algorithms to implement. I
would absolutely appreciate some help here.

ftlString is incomplete. I would like to have all Python string manipulation
methods, but only a handful are implemented at the moment. Help is much
appreciated. Conversion from numeric types to ftlString also needs to be
implemented. Fortran 2003 derived-type I/O would be nice, though this is not yet
supported in gfortran at the moment.

Definitely on the TODO list are:

+ A wrapper container for an existing plain Fortran array. It should provide a
  random access iterator so that the ftlAlgorithms can work on plain Fortran
  arrays.

+ An equivalent of std::deque, a double-ended queue. A container with random
  access iterators but constant time insertion at both ends. It should be
  reasonably local in memory.

+ An equivalent of std::set. Should probably share code with ftlHashMap, as it
  is essentially a hash table without a value associated with the keys.

These things might be nice:

+ A regular expression library. Probably best to just make a Fortran interface
  to the C regex library ...

+ Random number generators and distributions like in std::random.


License
#######

The Fortran Template Library is published under the GNU Lesser General Public
License. This should permit virtually any use, including the use in closed
source software according to section 3 ("Object Code Incorporating Material from
Library Header Files") of the GNU Lesser General Public License.
