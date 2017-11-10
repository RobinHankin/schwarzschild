\name{standard_colours}
\alias{standard_colours}
\alias{colors}
\alias{colour_critical}
\alias{colour_escaping}
\alias{colour_horizon}
\alias{colour_ingoing_light}
\alias{colour_outgoing_light}
\alias{colour_photon}
\alias{colour_r}
\alias{colour_raindrop}
\alias{colour_singularity}
\alias{colour_spacelike_curve}
\alias{colour_t}
\alias{colour_timelike_curve}
\docType{data}
\title{
Standard colours for spacetime diagrams
}
\description{
This file specifies colours for the various diagrams of the package.
The idea is to create a standard look-and-feel for all the diagrams in a
customisable way.

Not all the colours are used by all the plotting routines.
}
\usage{data("standard_colours")}
\format{
  The format is a named list:

\tabular{ll}{
ingoing_light        \tab red    \cr
outgoing_light       \tab blue   \cr
horizon              \tab magenta\cr
singularity          \tab black  \cr
singularity_interior \tab gray   \cr
r                    \tab green  \cr
t                    \tab orange \cr
photon               \tab red    \cr
timelike_curve       \tab black  \cr
spacelike_curve      \tab blue   \cr
raindrop             \tab black  \cr
cone_interior        \tab gray   \cr
cone_edge            \tab black  \cr
captured             \tab red    \cr
escaping             \tab blue   \cr
critical             \tab green
}
}
\examples{

schwarzschild()   # standard colours

col <- standard_colours
col$ingoing_light <- "blue"

schwarzschild(colors=col)    # ingoing light now blue


}
\keyword{datasets}
