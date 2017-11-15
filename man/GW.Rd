\name{GW}
\alias{GW}
\alias{animation}
\alias{GW_animation}
\title{Animation of a gravitational wave}
\description{Displays an animation of a gravitational wave passing
  through an array of test particles, perpendicular to the plane of the
  plot}
\usage{
GW(plus = 0.03, cross = 0, lattice = TRUE, frames = 20, n = 11, center = c(0, 0))
}
\arguments{
  \item{plus,cross}{(Complex) amplitudes for the plus and cross polarizations}
  \item{frames}{Number of frames in one oscillation}
  \item{lattice,n}{If argument \code{lattice} is \code{TRUE}, the particles are
    arranged in a \eqn{n\times n}{n by n} lattice; if \code{FALSE}, there
    are \eqn{n} particles arranged in a circle}
  \item{center}{Coordinates of point which appears to be at rest (but
    note that, in reality, all the particles are at rest)}
}
\details{
  Does what it says on the tin, basically.  Creates a looped animation of
  a gravitational wave passing.  See the examples.
}
\author{Robin K. S. Hankin}
\examples{


\dontrun{
GW()  # default gravitational wave with the plus polarization

GW(plus=0, cross=0.03)  # cross polarization

GW(plus=0.03,cross=0.03)  # both together
GW(plus=0.03,cross=0.06)  # both together

GW(plus=0.05,cross=0.05i) # circular polarization (note 'i')
GW(plus=0.02-0.01i,cross=0.03+0.02i) # complicated


GW(lattice=FALSE)
GW(plus=0.03,cross=0.03i, lattice=FALSE)


GW()
GW(center=c(10,10))

}

}

  