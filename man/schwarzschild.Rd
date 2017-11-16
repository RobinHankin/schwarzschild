\name{schwarzschild}
\alias{schwarzschild}
\alias{eddington}
\alias{eddington_outgoing}
\alias{gullstrand}
\alias{kruskal}
\alias{kruskal_extended}
\alias{kruskal_inverted}
\alias{kruskal_with_throw}
\alias{lemaitre}
\title{Black hole diagrams}
\description{Black hole diagrams using different coordinate systems,
  including Schwarzschild coordinates}
\usage{
schwarzschild(draw_infalling_drops = FALSE, colours=standard_colours, ...)
gullstrand(draw_infalling_drops = FALSE, colours=standard_colours, ...)
eddington(colours=standard_colours, ...)
eddington_outgoing(colours=standard_colours, ...)
kruskal(colours=standard_colours, ...)
kruskal_with_throw(draw_constant_schwarzschild = FALSE, label_curves = FALSE, colours=standard_colours)
kruskal_extended(colours=standard_colours, ...)
kruskal_inverted(colours=standard_colours, ...)
lemaitre(draw_schwarzschild=FALSE, colours=standard_colours, ...)
}
\arguments{
  \item{draw_infalling_drops, draw_constant_schwarzschild,
    label_curves, draw_schwarzschild}{Boolean, with \code{TRUE}
    meaning to plot the thing, and default
    \code{FALSE} meaning to omit the lines (resulting in a tidier and
    less cluttered diagram).  }
  \item{colours}{List of colours}
  \item{...}{Further arguments, currently ignored}
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{penrose}}}
\examples{

schwarzschild()
schwarzschild(TRUE)

kruskal()
kruskal_extended()
kruskal_with_throw()

eddington()
eddington_outgoing()

\dontrun{lemaitre()}

gullstrand()

thrower('','topright')
thrower('x')
thrower('xy')

}

