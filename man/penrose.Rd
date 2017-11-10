\name{penrose}
\alias{penrose}
\alias{penrose_cauchy}
\alias{penrose_laplace}
\alias{penrose_logistic}
\alias{penrose_norm}
\alias{penrose_BH}
\alias{penrose_BH_cauchy}
\alias{penrose_BH_laplace}
\alias{penrose_BH_logistic}
\alias{penrose_BH_norm}
\alias{penrose_BH_extended}
\title{
Penrose transforms of the universe to a compact diagram.
}
\description{
Penrose transforms of the universe to a compact diagram.  The
\dQuote{BH} forms include a black hole.
}
\usage{
penrose_cauchy(colours=standard_colours, ...)
penrose_laplace(colours=standard_colours, ...)
penrose_logistic(colours=standard_colours, ...)
penrose_norm(colours=standard_colours, ...)
penrose_BH_cauchy(colours=standard_colours, ...)
penrose_BH_laplace(colours=standard_colours, ...)
penrose_BH_logistic(colours=standard_colours, ...)
penrose_BH_norm(colours=standard_colours, ...)
penrose_BH_extended(colours=standard_colours, ...)
}
\arguments{
  \item{colours}{colours argument, a list}
  \item{\dots}{Further arguments, currently ignored}
}
\details{
Standard Penrose transform, no black hole
}
\value{
Executed for its side-effect of plotting a graph
}
\author{Robin K. S. Hankin}
\examples{

penrose_cauchy()
penrose_laplace()
penrose_logistic()
penrose_norm()

penrose_BH_cauchy()
penrose_BH_laplace()
penrose_BH_logistic()
penrose_BH_norm()

penrose_BH_extended()
}