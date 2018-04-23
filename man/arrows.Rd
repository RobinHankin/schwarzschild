\name{arrows}
\alias{arrows}
\alias{RT}
\alias{TX}
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
raindrop_arrow(r,delta,offset=0, ...)
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
  
  These functions take an \code{offset} argument that makes it easy to
  place several arrows on the same null geodesic (or worldline in the
  case of \code{raindrop()}).
    
  The idea is that

  \itemize{
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2, offset=3)}
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2, offset=4)}
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2, offset=5)}
  }
  
  give arrows which are vertically aligned at \eqn{r=2}; while:

  \itemize{
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2.0, offset=3)}
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2.3, offset=4)}
    \item\code{ingoing_null_arrow_eddington_outgoing_coords(r=2.6, offset=5)}
  }
  
  will give arrows which are all on the same null geodesic.
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

