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

Once this function template is defined one can just call the function.

.. code:: c++

    bigger_integer = max(1,6)
    bigger_float   = max(1.2,6.4)

The compiler will determine that the type ``T`` is ``int``  for the first call
and ``float`` for the second and will generate two versions of the max function
template: One for ``int`` and ``float``. This is called template instantiation.
This does not only work for the plain old types like numbers, but for ``any``
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
