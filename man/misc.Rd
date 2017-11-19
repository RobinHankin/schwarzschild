\name{misc}
\alias{misc}
\alias{RT}
\alias{TX}
\alias{bounded_outward_trajectory}
\alias{unbounded_outward_trajectory}
\alias{cone}
\alias{lightcone}
\alias{ingoing}
\alias{ingoing_light}
\alias{ingoing_null_arrow}
\alias{ingoing_trajectory}
\alias{inward_trajectory}
\alias{null_geodesics}
\alias{outgoing}
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
    \item{t,rzero,tzero}{In functions \code{ingoing() and \code{outgoing()},
    \code{t} is (Schwarzschild) time, and \code{tzero} and \code{rzero} specify an even      through which the null geodesic passes}
    
    
}
\details{

Function \code{lightcone()} plots a nice little light cone on a plot.
It is more user-friendly than the lower-level function \code{cone()}.

Functions \code{raindrop()} and \code{raindrop_arrow()} are used in
\code{schwarzschild()} to draw worldlines of particles falling from rest
at infinity.  Note that functions \code{gullstrand()} and
\code{lemaitre()} have options to plot raindrops but use unrelated
internal code to do so.

Function \code{penrose_transform()} returns one of several Penrose
transformations.  The forward transform maps the real line to
\eqn{(-1,1)} and the backward transform is the inverse of this.

Function \code{ingoing()} specifies ingoing light curves, as a function
of (Schwarzschild) t, passing through event (\code{rzero},\code{tzero}).
The difficult bit is dealing with the qualitatively different behaviour
inside and outside the Schwarzschild radius (note that this function, as
do all the black hole functions, use units in which \eqn{2GM/c^2=1}).
The first argument of this function is (Schwarzschild) time \eqn{t}.
Note that, in Schwarzschild coordinates, ingoing null geodesics are
either always inside, or always outside, \eqn{r=1}.

Function \code{outgoing()} is the same but for outgoing null geodesics.



}
\author{Robin K. S. Hankin}
