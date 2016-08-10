# yentl

Yentl is a synthetic geometry DSL in Haskell.

We can think of synthetic Euclidean geometry as a declarative language. With an appropriate interpreter, statements in this language can be "executed" to produce drawings. Yentl is intended to act as such an interpreter. This project is a companion to my College Geometry class notes, available [here](http://nbloomf.github.io/pages/geo-notes.html).

The DSL consists of a few basic parts.

* A handful of type classes which model the technology available in geometries of various kinds (like *lines*, *angles*, *incidence*, *betweenness*, and *congruence*).
* Instances of these type classes representing (at the moment) the cartesian plane, the poincare half-plane, and the poincare disc. More models to come. These are implemented using exact arithmetic, so that, for example, we can ask whether two points are the same and be certain that the answer is correct. (Floating point arithmetic can't do this!)
* A monad (``Fig``) that handles errors, logging, and IO behind the scenes.
* A small but growing library of constructions and theorems, ready to use.
* A suite of tests that checks our theorems in each model.

At the moment documentation is nonexistent, but there are several examples in ``src/Yentl/Demo.hs`` and ``scripts/``.

## Example

Here is an example drawing: this demonstrates how to copy a given segment (in blue) onto a given ray (in green). This example is ``scripts/test002.hs``.

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

Well, almost -- the script makes an eps (vector) image, which I've converted to png here.

The type signature specifies that this construction takes place in the ordinary Cartesian plane model of euclidean geometry. Changing the signature to

    Fig PoincarePlane ()

makes the construction happen in the Poincare half-plane model.

![Demo image](/doc/gfx/readme-ex2.png)

This figure description is actually copied from the theorem library. There, it is wrapped up in a function called ``copySegmentToRay``, and can be used to build more complicated figures. It also shows up in the test suite, where the construction can be checked (exactly) in each model on random inputs.

## Contents

This package provides the Yentl library and two programs.
* ``yentl-test`` is the automated test suite, which is built using QuickCheck and Tasty. Invoking this program with no options will run each test on 100 random inputs. (Watch out! Some tests take a while.) Several other options are available to tweak the tests; run ``yentl-test --help`` for a list. The most important of these are
  * ``--quickcheck-tests NUM``, which lets us set the number of random inputs to test, and 
  * ``--num-threads NUM``, which lets us run the tests concurrently with ``NUM`` threads.
* ``yentl-demo`` builds a directory ``demo`` and populates it with example images described with yentl. Some of these are static images and others are frames meant to be compiled into animations. Animated examples come with a ``makefile`` that does the compiling; you'll need ``imagemagick`` for this.

## Features

* All arithmetic is exact. This is slow but eliminates a large class of errors.
* Diagrams are written first to a very small internal representation and then converted to EPS. Other output formats can be implemented by writing a converter from the IR.

## Using yentl

The simplest way to use yentl is in a standalone haskell module interpreted as a script with ``runhaskell``; see ``/scripts`` for some examples. With this style of use, even though yentl is a Haskell library, individual figures or animations can be described and evaluated without needing to compile anything or make a cabal file.
