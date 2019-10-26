\name{logo}
\alias{logo}
\alias{AUT-logo}
\alias{AUT-logo-block}
\alias{AUT-logo-block.ps}
\alias{AUT-logo-block.pdf}
\alias{AUT-logo-block.ps.xml}
\alias{git}
\alias{github}
\alias{githublogo}
\title{Add the AUT logo to an image}
\description{Add the AUT logo to an image using grImport}
\usage{
logo(...)
git(x,y,...)
}
\arguments{
  \item{\dots}{In function \code{logo()}, further arguments passed to
    \code{grid.picture()}, and in function \code{git()} passed to
    \code{text()}}
  \item{x,y}{In function \code{git()}, the coordiates of the position to
    put the github repo URL}
}
\note{
  The logo file in the package is \code{AUT-logo-block.ps.xml}, created
  using \code{PostScriptTrace()}.  The logo is plotted unless option
  \code{AUTlogo} is \code{FALSE}.

  In most plotting functions in the package, the appearance of the logo
  is controlled by \code{options("schwarzschild_logo")} which is
  interpreted as the filename of an \code{xml} file in the \code{inst/}
  directory.  There are a few special values: default \code{NULL} means
  to include a fivefold symmetrical ornamental knot
  (\dQuote{\code{orn20.ps.xml}}), and \code{FALSE} means to plot
  nothing.  Passing \code{AUT-logo-block.ps.xml} plots the AUT logo.

  Function \code{git()} uses \code{text()} to show the github repo URL.

  The plotting functions documented here are designed to be used to plot
  \code{PDF} diagrams with \code{pdf(..., height=9,width=9)}; changing
  this could adversely affect the appearance (for example, the logo
  might appear in a suboptimal place).
}

\author{Robin K. S. Hankin}
\examples{
schwarzschild()
logo(x=0.5,y=0.5,width=0.5)  # a bit big...
git(1,1,col='red')

}
