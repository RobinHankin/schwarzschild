---
title: 'General relativity in R: a set of mathematically precise diagrams of black holes'
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
affiliations:
- index: 1
  name: Auckland University of Technology
nocite: |
@schutz2009, @carroll2019
---

# Introduction


In general relativity, Schwarzschild coordinates for a black hole have
desirable properties such as asymptotic matching with flat-space
spherical coordinates; but other coordinate systems can be used which
have other advantages.  The schwarzschild package furnishes plots
which use a variety of coordinates including Kruskal-Szekeres
[@kruskal1960;@szekeres1960], Eddington-Finkelstein
[@eddington1924;@finkelstein1958], Gullstrand-Painleve [@painleve1921;
@gullstrand1922], Lemaitre [@lemaitre1933], and various Penrose
diagrams with or without a black hole [@hawking1973].  These are
described in many undergraduate GR textbooks such as Schutz (2009) and
Carroll (2019).


# Statement of Need

In the teaching of numerical subjects such as physics, mathematical
accuracy is an important requirement for informative diagrams.
However, in the case of general relativity, mathematically accurate
diagrams do not seem to be available to educators under a free
license.  The `schwarzschild` package, available under the GPL, fills
this need by creating camera-ready PDF diagrams of black holes using a
range of coordinate systems.

The images have been used in two general relativity teaching contexts:
firstly, as resources for my own astrophysics lectures at AUT, and
secondly to support my ``Trin Tragula'' YouTube channel [General
relativity step by
step](https://www.youtube.com/watch?v=JzCX3FqDIOc&list=PL9_n3Tqzq9iWtgD8POJFdnVUCZ_zw6OiB)
which as of 2019 has over 48000 views and 770 subscribers.

# Functionality and usage

The ``schwarzschild`` package creates high-quality PDFs that
illustrate different aspects of physics near a spherically symmetric
black hole.  Although the main function of the package is the PDF
images themselves, the physics underlying the Schwarzschild black hole
is illustrated by the R functionality.

The package defines over twenty functions that are called for their
side-effect of plotting a diagram of spacetime in the vicinity of a
black hole.  The code itself is heavily documented, and makes the
connection between physics and plotted diagram explicit.  The package
is written to behave well in the wider ecology of R software.  



# References
