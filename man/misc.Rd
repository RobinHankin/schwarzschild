\name{misc}
\alias{misc}
\alias{RT}
\alias{TX}
\alias{bounded_outward_trajectory}
\alias{unbounded_outward_trajectory}
\alias{cone}
\alias{lightcone}
\alias{ingoing}
\alias{outgoing}
\alias{ingoing_lambert}
\alias{outgoing_lambert}
\alias{ingoing_null_arrow}
\alias{ingoing_trajectory}
\alias{inward_trajectory}
\alias{ingoing_light}
\alias{null_geodesics}
\alias{outgoing_null_arrow}
\alias{outward_inside_trajectory}
\alias{penrose_transform}
\alias{raindrop}
\alias{raindrop_arrow}
\alias{thrower}
\alias{thrower_asp1}
\alias{trajectory}
\title{Miscellaneous helper functions}
\description{
Various helper functions that should not appear in the plotting routines
}
\usage{
RT(TX)
TX(rt,exterior)
cone(x, y, leftangle, rightangle, size = 0.2, colours=standard_colours, ...)
lightcone(x, y, size, colours = standard_colours) 
raindrop(r)  
raindrop_arrow(r,delta,offset=0, ...)
penrose_transform(choice, forward)
ingoing(r,rzero,tzero=0)
outgoing(r,rzero,tzero=0)
ingoing_lambert(t,rzero,tzero=0)
outgoing_lambert(t,rzero,tzero=0)
}
\arguments{
  \item{TX,rt}{Two-column matrix giving time and space}
  \item{exterior}{Boolean, specifying whether the point in question is
    inside or outside the event horizon}
  \item{x, y, leftangle, rightangle, size, colours, ...}{Arguments for
    \code{cone()} and \code{lightcone()} specifying features of a light
    cone}
  \item{choice,forward}{In function \code{penrose_transform()} argument
    \code{choice} is a string specifying the name of the statistical
    distribution used to define the transform.  Current choices are
    Cauchy, normal (Gaussian), logistic, laplace; see details section.
    Argument \code{forward} is Boolean (no default) with \code{TRUE}
    meaning to return the forward transformation and \code{FALSE}
    meaning to return the backward transformation.}
  \item{r,t}{Schwarzschild radius and Schwarzschild time}
  \item{rzero,tzero}{In functions \code{ingoing()} and
    \code{outgoing()}, arguments \code{tzero} and \code{rzero} specify
    an event through which the null geodesic passes.  These functions
    return \code{t} as a function of \code{r}.  Functions
    \code{ingoing_lambert()} and \code{outgoing_lambert()} use Lambert W
    function (of the \pkg{gsl} package) to take \code{t} and return
    \code{r}}
  \item{delta,offset}{In functions \code{raindrop()} and
    \code{raindrop_arrow()}, details of the position of the line and the
    arrow}
}
\details{
  \itemize{
    
    \item Function \code{lightcone()} plots a nice little light cone on
    a plot.  It is more user-friendly than the lower-level function
    \code{cone()}.
    
    \item Functions \code{raindrop()} and \code{raindrop_arrow()} are
    used in \code{schwarzschild()} to draw worldlines of particles
    falling from rest at infinity.  Note that functions
    \code{gullstrand()} and \code{lemaitre()} have options to plot
    raindrops but use unrelated internal code to do so.

    \item Function \code{penrose_transform()} returns one of several
    Penrose transformations.  The forward transform maps the real line
    to the interval \eqn{(-1,1)} and the backward transform is the
    inverse of this.  Argument \code{choice} specifies a statistical
    distribution.  The forward transform uses the distribution's
    cumulative distribution function (CDF) \eqn{F(\cdot)}{F(.)}, but
    because the CDF maps to \eqn{(0,1)} rather than the desired
    \eqn{(-1,1)} it maps \eqn{x} to \eqn{2F(x)-1}.  The backward
    transform uses the distribution's quantile function
    \eqn{Q(\cdot)}{Q(.)}, mapping \eqn{x} to \eqn{Q((x+1)/2)}.

    \item Functions \code{ingoing()} and \code{outgoing()} give the
    ingoing or outgoing light curve passing through event
    (\code{rzero},\code{tzero}).  They take (Schwarzschild) radial
    coordinate \eqn{r} and return (Schwarzschild) \eqn{t}.  Note that,
    in Schwarzschild coordinates, radial null geodesics are either
    always inside, or always outside, \eqn{r=1} (this applies to both
    ingoing and outgoing light).

    \item Functions \code{ingoing_lambert()} and
    \code{outgoing_lambert()} do the same physics as \code{ingoing()}
    and \code{outgoing()}, but take (Schwarzschild) time \eqn{t} as an
    input and return (Schwarzschild) radius \eqn{r}.
  }

}
\author{Robin K. S. Hankin}
