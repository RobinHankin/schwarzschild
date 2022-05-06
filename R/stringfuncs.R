`u0` <- function(r,K=0){r-K}  # here for completeness

`u1`  <- function(r,K=0){  # equal string lengths
  jj <- sqrt((r-1)/r)
  r*jj + log((1+jj)/(1-jj))/2 - K
}	

`u2` <- function(r,K=0){  # equal (annular) areas
  jj <- sqrt((r-1)/r)
  int <- -r^2/4*(3*jj^3 - 5*jj)  + (3/8)*log((1+jj)/(1-jj))
  sqrt(2*(int-K))
}	

`u3` <- function(r,K=0){  # equal (annular) areas
  jj <- sqrt((r-1)/r)
  int <- r^3*(15*jj^(5/2) - 40*jj^(3/2) + 33*jj)/24 + (5/16)*log((1+jj)/(1-jj))
  (3*(int-K))^(1/3)	  
}	

`u0_inv` <- function(u){  # returns Schwarzschild r, here for completeness
  u
}

`u1_inv` <- function(u){  # returns Schwarzschild r
  uniroot(function(r){u1(r)-u},c(1,100))$root
}

`u2_inv` <- function(u){  # returns Schwarzschild r
  uniroot(function(r){u2(r)-u},c(1,100))$root
}

`u3_inv` <- function(u){  # returns Schwarzschild r
  uniroot(function(r){u3(r)-u},c(1,100))$root
}