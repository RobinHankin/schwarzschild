`edstring` <- function(colours=standard_colours, ...){
    `sl` <- function(r){  # string length
        A <- sqrt(r)
        B <- sqrt(r-1)
        
        A*B +log((A+B)/(A-B))/2
    }
    
  n <- 4  # size of plot
  
  plot(NULL,
       xlim=c(0,n),ylim=c(0,n),ylab='',xlab='',
       axes=FALSE,asp=1,
       main="Eddington-Finkelstein/string coordinates (ingoing)")

  axis(1,pos=0,at=0:4)
  axis(2,pos=0)



  par(xpd=TRUE)
  text(2,-0.5,'string')
  text(-0.5,2,'tdash',srt=90)

  par(xpd=FALSE)

  clip(0,n,0,n)
    abline(v=sl(1),lwd=5,col=colours$horizon)
  abline(v=c(0.5,1,2,3,4),lwd=0.5,col=colours$r)

  small <- 1e-4

  rout <- sl(seq(from=1+small,to=n,len=1000))
  stringout <- sl(rout)

  time <- -20:20
  for(i in time){
    
    jj <- cbind(stringout,tdash = i+log(abs(rout-1)))
    points(jj,type='l',lwd=1/2,col=colours$t)
    
    jj <- cbind(stringout,tdash = i+rout+2*log(abs(rout-1)))
    points(jj,type='l',col=colours$outgoing_light)
  }

  for(i in seq_len(7)){
      points(stringout,i-rout,type="l",col=colours$ingoing_light)
  }

  par(xpd=TRUE)
  par(lend=1)
  legend(
      x=2.1, y=n-1.1,
      bg='white',
      lwd=5, col=c(colours$singularity,colours$horizon),
      legend=c("singularity","horizon")
  )
  
  legend(
      x=2.1,y=n-0.3,lty=1,bg='white',
      col = c(colours$ingoing_light,colours$outgoing_light,colours$r,colours$t),
      legend=c(
          "ingoing light",
          "outgoing light",
          "lines of constant r",
          expression("lines of constant t"["schwarz"]))
  )


  ## plot the AUT logo:
  if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.8,y=0.08, width=0.1)}  

  git(-0.8,-0.7)

}
