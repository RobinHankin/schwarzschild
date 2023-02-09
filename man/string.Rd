\name{string}
\alias{string}
\alias{u0}
\alias{u1}
\alias{u2}
\alias{u3}
\alias{u0_inv}
\alias{u1_inv}
\alias{u2_inv}
\alias{u3_inv}
\title{Convert to and from string coordinates}
\description{\loadmathjax
  Convert to and from string coordinates
  }
\usage{
u0(r, K = 0)
u1(r, K = 0)
u2(r, K = 0)
u3(r, K = 0)
u0_inv(u)
u1_inv(u)
u2_inv(u)
u3_inv(u)
}
\arguments{
  \item{r,K}{In the forward functions, Schwarzschild radius and constant
    of integration respectively}
  \item{u}{String coordinate}
}
\details{
Convert between Schwarzschild (circumferential) r and string coordinates u.

Function \code{u1(R)} is the proper length of a piece of string
stretching from the event horizon (\mjseqn{r=1}) to R.  Symbolically

\mjdeqn{
  u=\int_{s=1}^r\frac{ds}{\sqrt{1-1/s}}
  = \sqrt{r(r-1)} + \log\left(\sqrt{r} + \sqrt{r-1}\right)
}{
  omitted; see PDF
}

(there is an unavoidable notational infelicity here because the symbol
\dQuote{\mjseqn{r}} has two non-negotiable interpretations: it is both
the integration variable, and the upper limit of the definite integral).

Documentation for \code{u2()} and \code{u3()} to be added.

The R idiom for functions \code{u1()} through \code{u3()} uses an
algebraic trick to ensure that only a single square root is taken, for
reasons of efficiency.

}
\value{All forward functions are vectorised}
\references{
Hankin 2020
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{eddington_stringcoords}}}
\examples{


u1(1)
u1(1.001)

}


