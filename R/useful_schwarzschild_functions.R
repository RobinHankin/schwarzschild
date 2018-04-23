`ingoing` <- function(r,rzero, tzero=0){    # ingoing light curves, as a function of (Schwarzschild) r, passing through event (rzero,tzero)
    -(r + log(abs(r-1))) + tzero-rzero-log(abs(rzero-1))
}

`outgoing` <- function(r, rzero, tzero=0){    # ingoing light curves, as a function of (Schwarzschild) r, passing through event (rzero,tzero)
    +(r + log(abs(r-1))) + tzero-rzero-log(abs(rzero-1))
}

`ingoing_lambert` <- function(t,rzero,tzero=0){    # ingoing light curves, as a function of (Schwarzschild) t, passing through event (rzero,tzero)
  if(rzero>1){
    C <- tzero + rzero + log(rzero-1)
    return(1+lambert_W0(exp(C-1-t)))
  } else {
    C <- tzero + rzero + log(1-rzero)
    return(1+lambert_W0(-exp(C-1-t)))
  }
}

`outgoing_lambert` <- function(t,rzero,tzero=0){  # same as ingoing() but for outgoing light curves
  if(rzero>1){
    C <- -tzero + rzero + log(rzero-1)
    return(1+lambert_W0(exp(C-1+t)))
  } else {
    C <- -tzero + rzero + log(1-rzero)
    return(1+lambert_W0(-exp(C-1+t)))
  }
}

`cone` <- function(x,y,leftangle,rightangle,size=0.2,colours=standard_colours, ...){
  polygon(
      x=c(x,
          x-size*sin( leftangle),
          x+size*sin(rightangle)
          ),
      y=c(y,
          y+size*cos(leftangle),
          y+size*cos(rightangle)
          ),
      col=colours$cone_interior,
      border=NA
      )
          
  segments(x0=x,x1=x-size*sin( leftangle),y0=y,y1=y+size*cos(leftangle) ,lwd=2, col=colours$cone_edge, ...)
  segments(x0=x,x1=x+size*sin(rightangle),y0=y,y1=y+size*cos(rightangle),lwd=2, col=colours$cone_edge, ...)
}

`lightcone` <- function(x,y,size,colours=standard_colours){
  if(x>1){
    leftangle <- atan(1-1/x)
    rightangle <- atan(1-1/x)
  } else {
    leftangle <- -atan(1-1/x)
    rightangle <- pi-atan(1-1/x)
    }
  cone(x,y,leftangle=leftangle,rightangle=rightangle,size=size, colours=colours)
}

`raindrop` <- function(r){  # gives Schwarzschild t as a function of r for freely falling raindrops
  -2/3*sqrt(r)^3*(3/r+1) + log(abs((1+sqrt(r))/(1-sqrt(r))))
}

`raindrop_arrow` <- function(r,offset=0,...){
  ## draws an arrow on a raindrop worldline; cannot use usual offset trick as is it too difficult to differentiate raindrop()
  delta <- 0.001

  arrows(
      x0 = r,
      x1 = r-abs(delta),
      y0 = offset+raindrop(r),
      y1 = offset+raindrop(r-abs(delta)),
      angle=15, length=0.15, ...)
}

`outgoing_null_arrow_schwarz` <- function(r,offset,delta=0.001, colours=standard_colours, ...){

  arrows(
      x0 = r,
      x1 = r + sign(r-1)*delta,
      y0 = r + log(abs(r-1)) + offset,   # t
      y1 = r + log(abs(r-1)) + offset +delta*(1/abs(r-1)+sign(r-1)),
      angle = 15,
      length = 0.15,
      col = colours$outgoing_light,
      ...)
}

`ingoing_null_arrow_schwarz` <- function(r,offset,delta=0.001, colours=standard_colours, ...){
  arrows(
      x0 = r,
      x1 = r - delta,
      y0 = -r - log(abs(r-1)) + offset,   # t
      y1 = -r - log(abs(r-1)) + offset - delta*(-1-sign(r-1)/abs(r-1)),
      angle = 15, 
      length=0.15,
      col=colours$ingoing_light,
      ...)
}

`ingoing_null_arrow_eddington_ingoing_coords` <-
  function(r,offset, colours=standard_colours, ...){
    delta <- 0.001
    jjy <- -r + offset

    arrows(
        x0 = r,
        x1 = r - delta,
        y0 = jjy,
        y1 = jjy +  delta,
        angle = 15, 
        length=0.15,
        col=colours$ingoing_light,
        ...)
  }

`outgoing_null_arrow_eddington_ingoing_coords` <-
  function(r, offset, colours=standard_colours, ...){
    
    delta <- 0.001
    if(r<1){delta <- -delta}
    jjy <- offset+r+2*log(abs(r-1))
    
    arrows(
        x0 = r,
        x1 = r + delta,
        y0 = jjy,
        y1 = jjy + abs(delta*(1+2/(r-1))),
        angle = 15, 
        length=0.15,
        col=colours$outgoing_light,
        ...)
  }

`ingoing_null_arrow_eddington_outgoing_coords` <-
  function(r, offset, colours=standard_colours, ...){
    delta <- 0.001
    if(r>1){delta <- -delta}
    jjy <- offset-r-2*log(abs(r-1))
    arrows(
        x0 = r,
        x1 = r + delta,
        y0 = jjy,
        y1 = jjy + abs(delta*(1+2/(r-1))),
        angle = 15, 
        length=0.15,
        col=colours$ingoing_light,
        ...)
  }

`outgoing_null_arrow_eddington_outgoing_coords` <-
  function(r, offset, colours=standard_colours, ...){
    delta <- 0.001
    jjy <- r
    arrows(
        x0 = r,
        x1 = r + delta,
        y0 = jjy + offset,
        y1 = jjy + offset +  delta,
        angle = 15, 
        length=0.15,
        col=colours$outgoing_light,
        ...)
  }

