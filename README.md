# yentl

Yentl is a synthetic geometry DSL in Haskell.

We can think of synthetic Euclidean geometry as an abstract algorithmic language. With an appropriate interpreter, statements in this language may be interpreted as drawings. Yentl is intended to act as such an interpreter.

## Example

Here is an example drawing: this demonstrates how to copy a given segment (in blue) onto a given ray (in green).

```
fig :: Fig CartesianPlane ()
fig = do
  a <- coords (-1,1) >>= pen blue
  b <- coords (-2,2) >>= pen blue
  seg <- segment a b >>= pen blue

  o <- coords (2,3) >>= pen green
  p <- coords (3,5) >>= pen green
  ray <- ray o p >>= pen green

  c1 <- circle a b >>= pen ultrathin

  if a == o
    then do
      projectPointOnCircle p c1
      return ()
    else do
      (u,_) <- equilateralPoints a o
      pen plain u
      v <- pointBeyond u a
      w <- projectPointOnCircle v c1
      c2 <- circle u w >>= pen ultrathin
      s <- projectPointOnCircle o c2
      c3 <- circle o s >>= pen ultrathin
      projectPointOnCircle p c3 >>= pen red
      return ()
```

Running this script produces the following image.

![Demo image](/doc/gfx/readme-ex1.png)

The type signature specifies that this construction takes place in the ordinary Cartesian plane model of euclidean geometry. Changing the signature to

    Fig PoincarePlane ()

makes the construction happen in the Poincare half-plane model.

![Demo image](/doc/gfx/readme-ex2.png)

## Contents

This package provides the Yentl library and two programs.
* ``yentl-test`` is the automated test suite, which is built using QuickCheck and Tasty. Invoking this program with no options will run each test on 100 random inputs. (Watch out! Some tests take a while.) Several other options are available to tweak the tests; run ``yentl-test --help`` for a list. The most important of these are
  * ``--quickcheck-tests NUM``, which lets us set the number of random inputs to test, and 
  * ``--num-threads NUM``, which lets us run the tests concurrently with ``NUM`` threads.
* ``yentl-demo`` builds a directory ``demo`` and populates it with example images described with yentl. Some of these are static images and others are frames meant to be compiled into animations. Animated examples come with a ``makefile`` that does the compiling; you'll need ``imagemagick`` for this.

## Features

* All arithmetic is exact. This is slow but eliminates a large class of errors.
* Because all arithmetic is exact, we can (and do!) write a suite of automated tests of geometric theorems.
* Diagrams are written first to a very small internal representation and then converted to EPS. Other output formats can be implemented by writing a converter from the IR.

## Using yentl

The simplest way to use yentl is in a standalone haskell module interpreted as a script with ``runhaskell``; see ``/scripts`` for some examples. With this style of use, even though yentl is a Haskell library, individual figures or animations can be described and evaluated without needing to compile anything or make a cabal file.
