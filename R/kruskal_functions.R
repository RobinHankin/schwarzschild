## This is used by ./kruskal/kruskal.R and ./penrose_black_hole/pbr.R


library(gsl)

## TX() effects the transformation from the Schwarzschild coordinates
## (r,t) to KS coordinates (X,T):
`TX` <- function(rt,exterior){
  r <- rt[,1,drop=TRUE]
  t <- rt[,2,drop=TRUE]

  if(exterior){
    out <- sqrt(r-1) * cbind(cosh(t/2),sinh(t/2))  # apparently the wrong way round but here we put X first to make plotting work in R
  } else {
    out <- sqrt(1-r) * cbind(sinh(t/2),cosh(t/2))
  }
  colnames(out) <- c("X","T")
  return(out*exp(r/2))
}


## RT() takes XT coords and returns Schwarzschild (r,t)
`RT` <- function(TX){   # reverse function, here for completeness
  T <- TX[,2,drop=TRUE]
  X <- TX[,1,drop=TRUE]

  out <- cbind(X,T) + NA   
  r <- 1+lambert_W0((X^2-T^2)/exp(1))

  wanted <- T^2 < 1+X^2 # corresponds to r>0

  T <- T[wanted]
  X <- X[wanted]
  r <- r[wanted]

  out[wanted,1] <- r
  out[wanted,2] <-  # Schwarzschild t:
    ifelse(r>=1,
           2*asinh(T/(sqrt(r-1)*exp(r/2))),
           2*acosh(T/(sqrt(1-r)*exp(r/2)))
           )
  
  return(out)
}
