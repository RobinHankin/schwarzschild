General relativity in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/schwarzschild)](https://cran.r-project.org/package=schwarzschild)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# Overview

The `schwarzschild` package creates a series of high-quality PDF images
showing different aspects of the physics of black holes.

# Installation

The package is not yet on CRAN. To install the current development
version use `devtools`:

    R> devtools::install_github("RobinHankin/schwarzschild")

And then to load the package use `library()`:

``` r
library("schwarzschild")
```

The PDF diagrams are created by `inst/maker.R` and a Makefile is
provided.

# Further information

For more detail, see the package vignette

    vignette("schwarzschild")
