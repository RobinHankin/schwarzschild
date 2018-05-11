\name{logo}
\alias{logo}
\alias{AUT-logo}
\alias{AUT-logo-block}
\alias{AUT-logo-block.ps}
\alias{AUT-logo-block.pdf}
\alias{AUT-logo-block.ps.xml}
\title{Add the AUT logo to an image}
\description{Add the AUT logo to an image using grImport}
\usage{
logo(...)
}
\arguments{
  \item{\dots}{Further arguments passed to \code{grid.picture()}}
}
\note{
  The file in the package is \code{AUT-logo-block.ps.xml}, created using
  \code{PostScriptTrace()}.
}
\author{Robin K. S. Hankin}
\examples{
schwarzschild()
logo(x=0.5,y=0.5,width=0.5)  # a bit big...
}