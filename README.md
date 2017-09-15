Template Function - When Code is a Function of Type
===================================================

In many application domains it is common to implement algorithms that
are defined for instances of the common lisp array, sequence and/or
number classes. The abstract nature of these classes makes it
difficult for compilers to simultaneously satisfy user demands on
run-time performance whilst maintaining generality. Unsurprisingly,
developers typically sacrifice generality by specializing the
implementation to one or more subclasses. This decision introduces
issues concerning maintainability, portability and reuse.

The template function system avoids this sacrifice by providing
functionality which allows users to request an implementation which is
specialized to a set of types. The generated specialization is then
associated with a template function object and is automatically
selected at runtime or at compile time according to type information
found in the function application.

The system provides support for template functions composed of other
template functions, recursive template functions, inlined templated
functions and template functions accepting optional, rest and keyword
arguments. Associated with this functionality is a meta-object
protocol consisting of object, glue and syntax layers.

# Installation

The template function system has the following dependencies:
1. [Alexandria](https://common-lisp.net/project/alexandria/)
2. [Introspect Environment](https://github.com/Bike/introspect-environment)
3. [Specialization Store](https://github.com/markcox80/specialization-store)
4. [FiveAM](https://common-lisp.net/project/fiveam/) (test framework)

The template function system has been tested
using [SBCL](http://www.sbcl.org)
and [Clozure Common Lisp](https://ccl.clozure.com).

Great effort has been spent on implementing this system such that it
is portable to all implementations. The compile time dispatch
functionality makes extensive use of compiler macros and the
[CLTL2 environment API](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html#SECTION001250000000000000000).

The template function system can be loaded in to your lisp environment
by evaluating the following [ASDF](https://common-lisp.net/project/asdf/)
forms
```lisp
(asdf:load-system "template-function")
```

The template function system includes a large suite of tests. These tests
can be executed by evaluating the following forms
```lisp
(asdf:test-system "specialization-store")
(asdf:test-system "template-function")
```

# Documentation

Documentation and tutorials for this project are provided in the
project wiki.
