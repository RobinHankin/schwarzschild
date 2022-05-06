`schwarz_stringcoords` <- function(draw_infalling_drops=FALSE,  colours=standard_colours, ...){

  n <- 4  # size of plot

  par(lend=1)

  plot(1:(n+1),0:n,xlim=c(0,n+0.1),ylim=c(0,n+0.1),asp=1,type='n',ylab='Schwarzschild t',axes=FALSE, main='Schwarzschild/string coordinates', xlab='string coord',...)
  axis(1,pos=0,at=0:n)
  axis(2,pos=0,at=0:n)

  clip(0,n,0,n)

  for(i in 1:4){
    segments(x0=i,y0=0,y1=n,col=colours$r,lwd=0.5,lty=1)  # horizontal lines; lines of constant r
    segments(x0=0,x1=n,y0=i,col=colours$t,lwd=0.5,lty=1)  # vertical lines; lines of constant t
  }

  r_outside <- seq(from=1.0001,to=n,len=1000) # "r" is Schwarzschild r

    xcoord <- u1(r_outside)
    ycoord <- ingoing(r_outside,rzero=0)

    ## Now ensure that (1,0) .... or ap(1) = 0
    ycoord <- ycoord - approxfun(xcoord,ycoord)(1)

  for(tz in -10:14){points(xcoord,tz+ycoord,type='l',lwd=0.5,col=colours$ingoing_light )}
  for(tz in  -3:14){points(xcoord,tz-ycoord,type='l',lwd=0.5,col=colours$outgoing_light )}
 
  if(draw_infalling_drops){
      xcoord <- u1(r_outside)
      ycoord <- raindrop(r_outside)-raindrop(u1_inv(1))
    for(i in -10:15){
      points(xcoord,ycoord+i, type='l',lty=5)
    }
  }

  abline(v=0,lwd=5,col=colours$horizon,par(lend=1))

  polygon(x=c(0,0,n+1,n+1),y=c(n,n+1,n+1,n),border=NA,lwd=7,col='white')

  if(draw_infalling_drops){
    legend(x=2.2, y=n-0.1, lty=c(1,1,1,1,1,5), bg='white',
           col=c(
               colours$ingoing_light,
               colours$outgoing_light,
               colours$r,
               colours$t,
               colours$horizon,
               'black'),
           legend=c(
               "ingoing light",
               "outgoing light",
               "lines of constant r",
               "lines of constant t",
               "horizon",
               "raindrops")
           )
  } else {
    legend(x=2.2, y=n-0.1, lty=1, bg='white',
           col=c(
               colours$ingoing_light,
               colours$outgoing_light,
               colours$r,
               colours$t,
               colours$horizon
           ),
           legend=c(
               "ingoing light",
               "outgoing light",
               "lines of constant r",
               "lines of constant t",
               "horizon"
           )
           )
  }
  
  points(cbind(1,0:4),pch=16)
  ## plot the AUT logo:
  if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.78,y=0.08, width=0.1)}

  git(-0.8,-0.7)

}
