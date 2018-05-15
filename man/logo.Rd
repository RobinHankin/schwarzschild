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

  Function \code{git()} uses \code{text()} to show the github repo URL.
  
}
\author{Robin K. S. Hankin}
\examples{
schwarzschild()
logo(x=0.5,y=0.5,width=0.5)  # a bit big...
git(1,1,col='red')

}