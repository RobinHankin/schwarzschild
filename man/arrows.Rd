\name{arrows}
\alias{arrows}
\alias{ingoing_null_arrow_schwarz}
\alias{outgoing_null_arrow_schwarz}
\alias{raindrop_arrow}
\alias{ingoing_null_arrow_eddington_ingoing_coords}
\alias{ingoing_null_arrow_eddington_outgoing_coords}
\alias{outgoing_null_arrow_eddington_ingoing_coords}
\alias{outgoing_null_arrow_eddington_outgoing_coords}

\title{Arrows on worldlines and null geodesics}
\description{
Helper functions that plot arrows on worldlines and null geodesics, with
functionality to make sure that the arrows are in the right direction
}
\usage{
raindrop_arrow(r,offset=0, ...)
ingoing_null_arrow_eddington_ingoing_coords(r, offset, colours=standard_colours, ...)
ingoing_null_arrow_eddington_outgoing_coords(r, offset, colours=standard_colours, ...)
outgoing_null_arrow_eddington_ingoing_coords(r, offset, colours=standard_colours, ...)
outgoing_null_arrow_eddington_outgoing_coords(r, offset, colours=standard_colours, ...)
}
\arguments{
  \item{r}{Schwarzschild radius}
  \item{offset}{Controls vertical position of arrow; see details}
  \item{colours}{List of colours}
  \item{...}{Further arguments, passed to \code{arrows()}}
}
\details{

  An arrow is drawn at event \eqn{(r,t)} where Schwarzschild \eqn{r} is
  specified by argument \code{r}.  The functions take an \code{offset}
  argument that ensures that ingoing or outgoing null geodesics reach
  the singularity at \code{tdash=offset}.  This makes it easy to place
  several arrows on the same null geodesic (or worldline in the case of
  \code{raindrop()}).
    
  The idea is that

  \itemize{
    \item\code{ingoing_null_arrow_eddington_ingoing_coords(r=2, offset=3)}
    \item\code{ingoing_null_arrow_eddington_ingoing_coords(r=2, offset=4)}
    \item\code{ingoing_null_arrow_eddington_ingoing_coords(r=2, offset=5)}
  }
  
  (executed while the output of \code{eddington()} is active the active
  device), plot arrows which are vertically aligned at \eqn{r=2} and are
  placed on ingoing null geodesics which reach the singularity at
  \code{tdash=3:5}.  Conversely:

  \itemize{
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2.0, offset=4)}
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2.3, offset=4)}
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2.6, offset=4)}
  }
  
  will give arrows which are all on the same null geodesic
  (specifically, the one that reaches the singularity at \code{tdash=4}.

}
\author{Robin K. S. Hankin}
\examples{

\dontrun{
eddington()
outgoing_null_arrow_eddington_ingoing_coords(r=2,offset= -1)
outgoing_null_arrow_eddington_ingoing_coords(r=2,offset=  0)
outgoing_null_arrow_eddington_ingoing_coords(r=2,offset= +1)
}
}

