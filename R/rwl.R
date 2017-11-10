## some functionality for geodesic motion in the vicinity of a black
## hole.  The challenging part is making sure that the ODE solver 

## This is all radial motion.

## in the work below, epsilon corresponds to 'energy' which is a
## conserved quantity in this case; epsilon=1 corresponds to an object
## with zero kinetic energy at infinity.

## part of the problem is that Schwarzschild t behaves badly at r=1
## and peculiarly at r<1.


## below, t0 and r0 represent the Schwarzschild t and r for the
## 'start' of the trajectory.  Boolean argument 'sign' says whether
## the initial trajectory is inward or outward (although this gloss is
## a bit weird for r<0).

## Function trajectory() is the user-friendly version which dispatches
## to one of the more specialized routines
## (bounded_outward_trajectory() etc) depending on the value of its arguments.

## The specialized routines use Euler's method which is, I know, inefficient.  


trajectory <- function(t0,r0,epsilon,sign,n=100){
  stopifnot(epsilon>0)
  stopifnot(sign %in% c(-1,1))

  if(r0<1){  # inside Schwarzschild radius
      jj <- outward_inside_trajectory(t0,r0,epsilon,n=n)
      if(sign<0){   # "inward" throw
          jj[,2] <- jj[1,2]-(jj[,2]-jj[1,2])  ## reverse time
      }
      return(jj)
  }
  
  if(sign>0){  # outward
    if(epsilon<1){  # bounded trajectory
      return(bounded_outward_trajectory(t0,r0,epsilon,n=n))
    } else {
      return(unbounded_outward_trajectory(t0,r0,epsilon,n=n))
    }
  } else {  # inward
    return(inward_trajectory(t0,r0,epsilon,n=n))
  }
}

bounded_outward_trajectory <- function(t0,r0,epsilon,n=100){  # sign>0,
  ## 0<epsilon<1 This function simulates the trajectory from (t0,r0),
  ## to (t_max,rmax) [aphelion, I guess]
    SMALL <- 1e-3
  stopifnot(epsilon>0)
  if(epsilon>1){
    stop("epsilon>1: trajectory unbounded (use unbounded_outward_trajectory()")
    }
    if(epsilon^2-(1-1/r0)<0){
    stop("epsilon is too small to allow the object to achieve radius r0.
 Need epsilon > sqrt(1-1/r)")
  }
  
  rmax <- -1/(epsilon^2-1)-SMALL
  r <- seq(from=r0,to=rmax,len=n)
  tee <- r*0 + t0
  for(i in 2:n){
    dr <- r[i]-r[i-1]
    tee[i] <- tee[i-1] + dr*epsilon/((1-1/r[i])*sqrt(abs(epsilon^2-(1-1/r[i]))) )
  }

    ## outward leg:
    out <- cbind(r=r,t=tee)

    ## now add return leg:
    out <- 

    out <- rbind(out, inward_trajectory(t0=tee[n],r0=r[n],epsilon,n=n))

  return(out)
}

unbounded_outward_trajectory <- function(t0,r0,epsilon,sign,rmax=10,n=100){
  stopifnot(epsilon>0)
  if(epsilon^2-(1-1/r0)<0){
    stop("epsilon is too small to allow the object to achieve radius r0.  Need epsilon > sqrt(1-1/r)")
  }
  
  r <- seq(from=r0,to=rmax,len=n)
  tee <- r*0 + t0
  for(i in 2:n){
    dr <- r[i]-r[i-1]
    tee[i] <- tee[i-1] + dr*epsilon/((1-1/r[i])*sqrt(abs(epsilon^2-(1-1/r[i]))) )
  }
  return(cbind(r=r,t=tee))
}


inward_trajectory <- function(t0,r0,epsilon,n=100){
    if(epsilon^2-(1-1/r0)<0){
        stop("epsilon is too small to allow the object to achieve radius r0.  Need epsilon > sqrt(1-1/r)")
    }
  r <- seq(from=r0,to=1,len=n)
  tee <- r*0 + t0
  for(i in 2:n){
      dr <- r[i]-r[i-1]

      tee[i] <- tee[i-1] - dr*epsilon/((1-1/r[i])*sqrt((epsilon^2-(1-1/r[i]))) )
  }
  return(cbind(r=r,t=tee))
  }

outward_inside_trajectory <- function(t0,r0,epsilon,n=n){
  r <- seq(from=r0,to=0,len=n)
  tee <- r*0 + t0
  for(i in 2:n){
      dr <- r[i]-r[i-1]
      tee[i] <- tee[i-1] + dr*epsilon/((1-1/r[i])*sqrt((epsilon^2-(1-1/r[i]))) )
  }
  return(cbind(r=r,t=tee))
}

