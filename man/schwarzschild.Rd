\name{schwarzschild}
\alias{schwarzschild}
\alias{classical}
\alias{schwarzschild_stringcoords}
\alias{eddington}
\alias{eddington_stringcoords}
\alias{eddington_outgoing}
\alias{gullstrand}
\alias{gullstrand_stringcoords}
\alias{kruskal}
\alias{kruskal_extended}
\alias{kruskal_inverted}
\alias{kruskal_with_throw}
\alias{lemaitre}
\alias{thrower}
\alias{thrower_asp1}
\title{Black hole diagrams}
\description{Black hole diagrams using different coordinate systems,
  including Schwarzschild coordinates}
\usage{
schwarzschild(draw_infalling_drops = FALSE, colours=standard_colours, ...)
classical(draw_infalling_drops = FALSE, colours=standard_colours, ...)
eddington(draw_infalling_drops=FALSE, colours=standard_colours, ...)
eddington_outgoing(draw_infalling_drops=FALSE, colours=standard_colours, ...)
eddington_stringcoords(draw_infalling_drops=FALSE, colours=standard_colours, ...)
gullstrand(draw_infalling_drops = FALSE, colours=standard_colours, ...)
kruskal(colours=standard_colours, ...)
kruskal_with_throw(draw_schwarzschild=FALSE, label_curves=FALSE, colours=standard_colours)
kruskal_extended(colours=standard_colours, ...)
kruskal_inverted(colours=standard_colours, ...)
lemaitre(draw_schwarzschild=FALSE, colours=standard_colours, ...)
thrower(logaxes,legpos="topleft", use_stringcoords=FALSE, colours=standard_colours, ...)
thrower_asp1(colours=standard_colours, ...)
}
\arguments{
  \item{draw_infalling_drops, label_curves, draw_schwarzschild}{Boolean,
    with \code{TRUE} meaning to plot the thing, and default
    \code{FALSE} meaning to omit the lines (resulting in a tidier and
    less cluttered diagram)}
  \item{colours}{List of colours}
  \item{legpos}{Passed to \code{thrower()} and \code{thrower_asp1()}
    specifying the position of the legend}
  \item{logaxes}{Passed to \code{thrower()} specifying which axes (if any)
    are logarithmic}
  \item{use_stringcoords}{Boolean, with \code{FALSE} meaning to use
    Schwarzschild circumferential radius \eqn{r} on the horizontal axis,
    and \code{TRUE} meaning to use string coordinate}
  \item{...}{Further arguments, generally passed to \code{plot()}}
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

