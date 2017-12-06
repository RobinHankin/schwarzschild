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
\alias{penrose_transform}
\title{
Penrose transforms of the universe to a compact diagram.
}
\description{
Penrose transforms of the universe to a compact diagram.  The
\dQuote{BH} forms include a black hole.
}
\usage{
penrose_transform(choice, forward)
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
  \item{choice,forward}{In function \code{penrose_transform()}, argument
    \code{choice} is a string specifying the name of the statistical
    distribution used to define the transform.  Current choices are
    Cauchy, normal (Gaussian), logistic, laplace; see details section.
    Argument \code{forward} is Boolean (no default) with \code{TRUE}
    meaning to return the forward transformation and \code{FALSE}
    meaning to return the backward transformation}
  \item{\dots}{Further arguments, currently ignored}
}
\details{
A Penrose transform uses a map from the real line to the interval
\eqn{(0,0)}.  Function \code{penrose_transform()} returns one of several
Penrose transformations.  The forward transform maps the real line to
the interval \eqn{(-1,1)} and the backward transform is the inverse of
this.  Argument \code{choice} specifies a statistical distribution.  The
forward transform uses the distribution's cumulative distribution
function (CDF) \eqn{F(\cdot)}{F(.)}, but because the CDF maps to
\eqn{(0,1)} rather than the desired \eqn{(-1,1)} it maps \eqn{x} to
\eqn{2F(x)-1}.  The backward transform uses the distribution's quantile
function \eqn{Q(\cdot)}{Q(.)}, mapping \eqn{x} to \eqn{Q((x+1)/2)}.
}
\value{
  Functions \code{penrose_foo()} and \code{penrose_BH_foo()} are
  executed for the side-effect of plotting a graph.  Function
  \code{penrose_transformation()} returns a function which gives the
  appropriate Penrose transform.
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