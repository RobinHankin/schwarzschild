## Defines null_geodesics_lemaitre(), which plots null geodesics
## nicely in Lemaitre cooordinates.  Function ingoing_light_lemaitre()
## is not used outside function null_geodesics_lemaitre().

ingoing_light_lemaitre <- function(x0,y0,outward=FALSE, times=seq(from=0,to=1,by=0.01)){

  parameters <- c(alpha = (2/3)^(2/3),outward=outward)
  state      <- c(X = x0, Y = y0)

  f <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      one <- -1      
      if(!outward){
        alpha <- -alpha
      }
      dX <- one
      dY <- -alpha/(X-Y)^(2/3)
      list(c(dX, dY))
    })
  }
  

  if(outward){ times <- -times}
  
  out <- ode(y = state, times = times, func = f, parms = parameters)
  X <- out[,2]
  Y <- out[,3]

  cbind(X[X>Y],Y[X>Y])
}

null_geodesics_lemaitre <- function(x0, y0, ingoing=TRUE, outgoing=TRUE, times=seq(from=0,to=1,by=0.01), colours=standard_colours, ...){
  if(ingoing){
    points(ingoing_light_lemaitre(x0,y0,outward=FALSE, times=times),type='l',col=colours$ingoing_light, ... )
  }

  if(outgoing){
    points(ingoing_light_lemaitre(x0,y0,outward=TRUE , times=times),type='l',col=colours$outgoing_light, ...)
  }
}
