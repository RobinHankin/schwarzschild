`classical` <- function(draw_infalling_drops=FALSE, colours=standard_colours, ...){

  n <- 4  # size of plot

  par(lend=1)

  plot(0:n,0:n,xlim=c(0,n+0.1),ylim=c(0,n+0.1),asp=1,type='n',ylab='classical t',axes=FALSE, main='Classical case', xlab='classical r',...)
  axis(1,pos=0,at=0:n)
  axis(2,pos=0,at=0:n)

  segments(x0=0,y0=0,y1=n, col=colours$singularity,lwd=5)
  
  clip(0,n,0,n)

  for(i in 1:4){
    segments(x0=i,y0=0,y1=n,col=colours$r,lwd=0.5,lty=1)  # horizontal lines; lines of constant r
    segments(x0=0,x1=n,y0=i,col=colours$t,lwd=0.5,lty=1)  # vertical lines; lines of constant t
  }


    ## ingoing outside, then outgoing outside, then ingoing inside, then outgoing inside:
    for(i in  1:7){segments(0, i, n, i-n,  lwd=0.5, col=colours$ingoing_light )}
    for(i in -3:3){segments(0, i, n ,i+n,  lwd=0.5, col=colours$outgoing_light)}




  if(draw_infalling_drops){

      r <- seq(from=0,to=n,by=0.1)
      for(i in 1:9){points(r, i+raindrop_classical(r), type='l', lty=5) }   # raindrop() defined in useful_schwarzschild_functions.R

      raindrop_arrow_classical(0.3,2)      #raindrop_arrow_classical() defined in useful_schwarzschild_functions.R
      raindrop_arrow_classical(1.4,2)      #raindrop_arrow_classical() defined in useful_schwarzschild_functions.R

  }

  ## draw arrows on some null curves:

  ingoing_null_arrow_classical(0.7,3,colours=colours)
  ingoing_null_arrow_classical(1.3,3,colours=colours)

  outgoing_null_arrow_classical(0.4,2,colours=colours)
  outgoing_null_arrow_classical(1.4,2,colours=colours)

  legend(
      x=2.2, y=n-1.1,
      bg='white',
      lwd=5, col=c(colours$singularity),
      legend=c("singularity")
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


  
  points(cbind(2,0:4),pch=16)
  ## plot the AUT logo:
  if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.78,y=0.08, width=0.1)}


  git(-0.8,-0.7)


}
