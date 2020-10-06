---
title: 'General relativity in R: visual representation of Schwarzschild space using different coordinate systems'
authors:
- affiliation: 1
  name: Robin K. S. Hankin
  orcid: 0000-0001-5982-0415
date: "November 4th, 2019"
output: pdf_document
bibliography: ref.bib
tags:
- Schwarzschild metric
- Schwarzschild solution
- general relativity
- black holes
- event horizon
- Eddington Finkelstein
- Kruskal-Szekeres coordinates
- Gullstrand-Painleve coordinates
- Penrose diagram
- Software infrastructure
affiliations:
- index: 1
  name: Auckland University of Technology
nocite: |
  @schutz2009, @carroll2019, @schwarzschild1916
---

# Introduction

In general relativity, Schwarzschild coordinates for a black hole have
desirable properties such as asymptotic matching with flat-space
spherical coordinates; but other coordinate systems can be used which
have other advantages such as removing the non-physical coordinate
singularity at the event horizon.  Following Schwarzschild's original
publication in 1916 of his spherically symmetrical solution to the
vacuum Einstein field equations, a variety of coordinate
transformations have been described that highlight different features
of the Schwarzschild metric.  These include: Kruskal-Szekeres
[@kruskal1960;@szekeres1960], Eddington-Finkelstein
[@eddington1924;@finkelstein1958], Gullstrand-Painleve [@painleve1921;
@gullstrand1922], Lemaitre [@lemaitre1933], and various Penrose
transforms with or without a black hole [@hawking1973].  These are
described in many undergraduate GR textbooks such as Schutz (2009) and
Carroll (2019).

# Statement of Need

In the teaching of numerical subjects such as physics, mathematical
accuracy is an important requirement for informative diagrams.
However, in this context a diagram should be viewed as the _end_ of a
process of calculation, not an object in its own right.  In computer
terminology one would need the source code as well as the final image
and such source code is not currently available.  The `schwarzschild`
R package [@rcore2019], available under the GPL, fills this need.  A
number of camera-ready PDF diagrams of black holes using a range of
coordinate systems are presented as examples of the software's
functionality; the software is extensively configurable to users'
requirements.

The software has been used in two general relativity teaching
contexts: firstly, as resources for my own astrophysics lectures at
AUT, and secondly to support my _Trin Tragula_ YouTube channel
[General relativity step by
step](https://www.youtube.com/watch?v=JzCX3FqDIOc&list=PL9_n3Tqzq9iWtgD8POJFdnVUCZ_zw6OiB)
which as of 2019 has over 48000 views and 770 subscribers.

# Functionality and usage

The `schwarzschild` package presents heavily documented and
structured R code that demonstrates different aspects of physics near
a spherically symmetric black hole.  Physical processes such as null
geodesics and freely falling objects are simulated near a
Schwarzschild black hole by the R functionality of the package; one
side-effect is the the creation of usable and accurate PDF images.

The package defines over twenty functions that are called for their
side-effect of plotting a diagram of spacetime in the vicinity of a
black hole.  The code itself is maintainable, extensible, and makes
the connection between physics and plotted diagram explicit.  The
package is written to behave well in the wider ecology of R software.


# References
