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
\alias{raindrop}
\alias{raindrop_arrow}
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
  \item{r,t}{Schwarzschild radius and Schwarzschild time}
  \item{rzero,tzero}{In functions \code{ingoing()} and
    \code{outgoing()}, arguments \code{tzero} and \code{rzero} specify
    an event through which the null geodesic passes}
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

    \item Functions \code{ingoing()} and \code{outgoing()} give the
    ingoing or outgoing light curve passing through event
    (\code{rzero},\code{tzero}).  They take (Schwarzschild) radial
    coordinate \eqn{r} and return (Schwarzschild) time \eqn{t}.  Note
    that, in Schwarzschild coordinates, radial null geodesics are either
    always inside, or always outside, \eqn{r=1} (this applies to both
    ingoing and outgoing light).

    These functions return \code{t} as a function of \code{r}.
    Functions \code{ingoing_lambert()} and \code{outgoing_lambert()} use
    Lambert W function (of the \pkg{gsl} package) to take \code{t} and
    return \code{r}}
}
\author{Robin K. S. Hankin}
