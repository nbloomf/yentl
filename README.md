# yentl

Yentl is a synthetic geometry DSL in Haskell.

We can think of synthetic Euclidean geometry as an abstract algorithmic language. With an appropriate interpreter, statements in this language may be interpreted as drawings.

This package provides the Yentl library and two programs.
* ``yentl-test`` is the automated test suite, which is built using QuickCheck and Tasty. Invoking this program with no options will run each test on 100 random inputs. (Watch out! Some tests take a while.) Several other options are available to tweak the tests; run ``yentl-test --help`` for a list. The most important of these are ``--quickcheck-tests NUM``, which lets us set the number of random inputs to test, and ``--num-threads NUM``, which lets us run the tests concurrently with ``NUM`` threads.
* ``yentl-demo`` builds a directory ``demo`` and populates it with example images described with yentl. Some of these are static images and others are frames meant to be compiled into animations. Animated examples come with a ``makefile`` that does the compiling; you'll need ``imagemagick`` for this.

## Features

* All arithmetic is exact. This is slow but eliminates a large class of errors.
* Because all arithmetic is exact, we can (and do!) write a suite of automated tests of geometric theorems.
* Diagrams are written first to a very small internal representation and then converted to EPS. Other output formats can be implemented by writing a converter from the IR.
