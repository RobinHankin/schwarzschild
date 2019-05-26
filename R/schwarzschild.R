schwarzschild <- function(draw_infalling_drops=FALSE, colours=standard_colours, ...){
  op <- par()   # Remember the options and reset at the end of the function
  n <- 4  # size of plot

  par(lend=1)

  plot(0:n,0:n,xlim=c(0,n+0.1),ylim=c(0,n+0.1),asp=1,type='n',ylab='Schwarzschild t',axes=FALSE, main='Schwarzschild coordinates', xlab='Schwarzschild r',...)
  axis(1,pos=0,at=0:n)
  axis(2,pos=0,at=0:n)

  segments(x0=0,y0=0,y1=n, col=colours$singularity,lwd=5)
  
  clip(0,n,0,n)

  for(i in 1:4){
    segments(x0=i,y0=0,y1=n,col=colours$r,lwd=0.5,lty=1)  # horizontal lines; lines of constant r
    segments(x0=0,x1=n,y0=i,col=colours$t,lwd=0.5,lty=1)  # vertical lines; lines of constant t
  }


  r_inside <- seq(from=0.0001,to=0.999,len=1000)
  r_outside <- seq(from=1.0001,to=n,len=1000)

  ## ingoing outside, then outgoing outside, then ingoing inside, then outgoing inside:
  for(tz in  0:14){points(r_outside, ingoing(r_outside,rzero=2,tzero=tz),type='l',lwd=0.5,col=colours$ingoing_light )}
  for(tz in -3:14){points(r_outside,outgoing(r_outside,rzero=2,tzero=tz),type='l',lwd=0.5,col=colours$outgoing_light)}
  for(tz in  0:12){points(r_inside,  ingoing(r_inside ,rzero=2,tzero=tz),type='l',lwd=0.5,col=colours$ingoing_light )}
  for(tz in  0:10){points(r_inside, outgoing(r_inside ,rzero=2,tzero=tz),type='l',lwd=0.5,col=colours$outgoing_light)}
  
  




  if(draw_infalling_drops){

    for(i in -2:4){points(r_inside  ,raindrop(r_inside)+i, type='l',lty=5) }
    for(i in -7:9){points(r_outside ,raindrop(r_outside)-raindrop(2)+i, type='l',lty=5) }

    raindrop_arrow(0.4,2)  
    raindrop_arrow(0.6,2)  
    raindrop_arrow(0.8,2)  

    raindrop_arrow(1.5, 1-raindrop(2))  # "1-" because object passes through (2,1)
    raindrop_arrow(1.5, 2-raindrop(2))  # "1-" because object passes through (2,1)
  }


  ## draw arrows on some null curves:

  outgoing_null_arrow_schwarz(0.56,2,colours=colours)
  outgoing_null_arrow_schwarz(0.40,2,colours=colours)

  outgoing_null_arrow_schwarz(0.56,3,colours=colours)
  outgoing_null_arrow_schwarz(0.40,3,colours=colours)

  outgoing_null_arrow_schwarz(2.50, 0,colours=colours)
  outgoing_null_arrow_schwarz(1.50, 0,colours=colours)
  outgoing_null_arrow_schwarz(1.10, 5,colours=colours)
  outgoing_null_arrow_schwarz(3.76,-1,colours=colours)

  ingoing_null_arrow_schwarz(0.56,1,colours=colours)
  ingoing_null_arrow_schwarz(0.40,1,colours=colours)

  ingoing_null_arrow_schwarz(0.56,2,colours=colours)
  ingoing_null_arrow_schwarz(0.40,2,colours=colours)

  ingoing_null_arrow_schwarz(1.81,3,colours=colours)
  ingoing_null_arrow_schwarz(1.45,3,colours=colours)
  ingoing_null_arrow_schwarz(3.76,8,colours=colours)



  abline(v=1,lwd=5,col=colours$horizon,par(lend=1))

  polygon(x=c(0,0,n+1,n+1),y=c(n,n+1,n+1,n),border=NA,lwd=7,col='white')

  ##  points(r_ingoing_inside ,t_ingoing_inside +i+0.5, type='l',col=colours$ingoing_light,lwd=0.3)
  ##  points(r_ingoing_outside,t_ingoing_outside+i+0.5, type='l',col=colours$ingoing_light,lwd=0.3)
  ##  points(r_outgoing_inside ,t_outgoing_inside +i+0.5, type='l',col=colours$outgoing_light,lwd=0.3)
  ##  points(r_outgoing_outside,t_outgoing_outside+i+0.5, type='l',col=colours$outgoing_light,lwd=0.3)
  

  


  legend(
      x=2.2, y=n-1.1,
      bg='white',
      lwd=5, col=c(colours$singularity,colours$horizon),
      legend=c("singularity","horizon")
  )
  
  if(draw_infalling_drops){
    legend(x=2.2, y=n-0.1, lty=c(1,1,1,1,5), bg='white',
           col=c(
               colours$ingoing_light,
               colours$outgoing_light,
               colours$r,
               colours$t,
               'black'),
           legend=c(
               "ingoing light",
               "outgoing light",
               "lines of constant r",
               "lines of constant t",
               "raindrops")
           )
  } else {
    legend(x=2.2, y=n-0.1, lty=1, bg='white',
           col=c(
               colours$ingoing_light,
               colours$outgoing_light,
               colours$r,
               colours$t),
           legend=c(
               "ingoing light",
               "outgoing light",
               "lines of constant r",
               "lines of constant t")
           )
  }
  
  lightcone(3.0,1,size=0.1)
  lightcone(2.0,1,size=0.1)
  lightcone(1.5,1,size=0.1)
  lightcone(0.8,1,size=0.1)
  lightcone(0.5,1,size=0.1)
  lightcone(0.2,1,size=0.1)


  points(cbind(2,0:4),pch=16)
  ## plot the AUT logo:
  if(!isFALSE(getOption("AUTlogo"))){logo(x=0.78,y=0.08, width=0.1)}


  git(-0.8,-0.7)
  par(op)  

}
