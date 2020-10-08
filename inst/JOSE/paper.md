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
R package [@rcore2019], available under the GPL, fills this need.  

The package is intended as a resource for lecturers of general
relativity and it is envisaged that the diagrams be used as visual
teaching aids for understanding the Schwarzchild metric.  A number of
camera-ready PDF diagrams of black holes using a range of coordinate
systems are presented as examples of the software's functionality; the
software is extensively configurable to users' requirements.

The software has been used in two general relativity teaching
contexts: firstly, as resources for my own astrophysics lectures at
AUT, and secondly to support my _Trin Tragula_ YouTube channel
[General relativity step by
step](https://www.youtube.com/watch?v=JzCX3FqDIOc&list=PL9_n3Tqzq9iWtgD8POJFdnVUCZ_zw6OiB)
which as of October 2020 has over 150000 views and 1200 subscribers.

# Functionality and usage

The `schwarzschild` package presents structured R code with extensive
inline documentation as part of an educational resource package.  The
package creates mathematically accurate diagrams illustrating
different aspects of physics near a spherically symmetric black hole.
Physical processes such as null geodesics and freely falling objects
are simulated near a Schwarzschild black hole by the R functionality
of the package; one side-effect is the the creation of usable and
accurate PDF images.


\newcommand\latexcode[1]{#1}

The package defines over twenty functions that are called for their
side-effect of plotting a diagram of spacetime in the vicinity of a
black hole.  The principal such function would be `schwarzschild()`,
which shows a spacetime diagram near a non-spinning stationary black
hole using Schwarzschild coordinates but many other coordinate systems
are available including Kruskal-Szekeres (`kruskal()` and variants),
\latexcode{Lema\^{\i}tre} coordinates (`lemaitre()`) and others.

The code itself is maintainable, extensible, and makes
the connection between physics and plotted diagram explicit.  The
package is written to behave well in the wider ecology of R software.

# Examples

Two example images from the 19 in the package vignette are shown below
in low-resolution jpg format.  For vectorized PDF, see the package
vignette.



![Low resolution image of space near a black
  hole, using standard Schwarzschild coordinates.
  Neither ingoing light (red) nor outgoing light (blue) crosses the
  event horizon (fuschia); inside the black hole, ingoing light
  travels backwards in Schwarzschild time but nears the singularity.
  As the event horizon is approached from the outside, the light cones
  (gray) close up but as the event horizon is crossed, they become
  everted.  As the singularity is approached, ingoing and outgoing
  light converge and meet the singularity horizontally.  For
  vectorized PDF version, see the package vignette or click the link
  below](schwarzschild.jpg)

![Low resolution image of maximally extended space near a black hole,
  using Kruskal-Szekeres coordinates.  Ingoing light (red) and
  outgoing light (blue) are represented as $45^\circ$ lines.  The
  event horizon (fuschia) is shown as a set of $45^\circ$ lines which
  show that neither ingoing nor outgoing light can escape the black
  hole and the singularity (thick black line) is unavoidable; the gray
  area above the singularity is meaningless.  Lines of constant
  Schwarzschild time are shown in orange and constant Schwarzschild
  radius in green; see how the lines of constant radius are spacelike,
  and lines of constant Schwarzschild time are timelike, inside the
  event horizon.  For vectorized PDF version, see the package
  vignette or click the link below](penrose_BH_extended.jpg)


# Downloads

The resulting PDF images are available at

[https://autuni-my.sharepoint.com/:f:/g/personal/rhankin_aut_ac_nz/EgX_IANsoOJDmTiH2i9_P20B6ksn9CMHf_TM31w5K3aITg?e=dPxAcp](https://autuni-my.sharepoint.com/:f:/g/personal/rhankin_aut_ac_nz/EgX_IANsoOJDmTiH2i9_P20B6ksn9CMHf_TM31w5K3aITg?e=dPxAcp)


# References
