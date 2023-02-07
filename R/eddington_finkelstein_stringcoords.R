`eddington_stringcoords` <- function(draw_infalling_drops=FALSE, colours=standard_colours, ...){

  n <- 4  # size of plot
  
  plot(NULL,
       xlim=c(0,n),ylim=c(0,n),ylab='',xlab='',
       axes=FALSE,asp=1,
       main="Eddington-Finkelstein/string coordinates (ingoing)")
  
  axis(1,pos=0,at=0:4)
  axis(2,pos=0)

  par(xpd=TRUE)
  text(2,-0.5,'string coordinates')
  text(-0.5,2,'tdash',srt=90)

  par(xpd=FALSE)
  clip(0,n,0,n)

  small <- 1e-4


  time <- -20:20

    ## Ingoing light:
    r <- seq(from=n,to=1,len=10000)
    t_edd <- n-r  # t_edd is eddington time

    points_ingoing_light <- shifter(cbind(u1(r),t_edd))  # shifter() defined in useful_schwarzschild_functions.R
    for(i in 0:(2*n+3)){
        jj <- points_ingoing_light

        jj[,2] <- jj[,2] + i
        points(jj,type="l",col=colours$ingoing)
        
        if(draw_infalling_drops){
          jj <- cbind(u1(r), raindrop(r) + (log(abs(r-1))))
          jj[,2] <- jj[,2] - approxfun(jj[,1],jj[,2])(1)  # ensures drop passes ingoing light pulses emitted from s=1
          jj[,2] <- jj[,2] + i-3
          points(jj,type="l",col=colours$raindrop,lty=2)
        }
    }

    ## outgoing light:
    r <- 1+sort(c(1/exp(seq(from=0,to=20,len=50)),
              seq(from=0.1,to=n,len=100)))  # need more points close to r=1

    points_outgoing_light <- shifter(cbind(u1(r),tdash=r+2*log(abs(r-1))))
    for(i in (-(2*n+13)):(2*n+10)){
        jj <- points_outgoing_light
        jj[,2] <- jj[,2] + i
        points(jj,type="l",col=colours$outgoing)
    }
    
    const_schwarz_t <- shifter(cbind(u1(r),tdash = log(abs(r-1))))
    for(i in -4:14){
        jj <- const_schwarz_t
        jj[,2] <- jj[,2] + i
        points(jj,type="l",lwd=1/2,col=colours$t)
    }


  ## ingoing light makes straight lines in Eddington-Finkelstein
  ## coordinates; we use string coords for the x-axis

  points(cbind(1,0:5),pch=16)
  abline(v=0,lwd=5,col=colours$horizon)

  jj_angle <- 21
  text(3.4,0.15,expression('t'['s']==0),col=colours$t,srt=jj_angle)
  text(3.4,1.15,expression('t'['s']==1),col=colours$t,srt=jj_angle)
  text(3.4,2.15,expression('t'['s']==2),col=colours$t,srt=jj_angle)
  text(3.4,3.15,expression('t'['s']==3),col=colours$t,srt=jj_angle)
  text(3.4,4.15,expression('t'['s']==4),col=colours$t,srt=jj_angle)
  text(3.4,5.15,expression('t'['s']==5),col=colours$t,srt=jj_angle)

  par(xpd=TRUE)
  par(lend=1)
  legend(
      x=1.8,y=n-0.3,lty=1,bg='white',
      col = c(colours$ingoing_light,colours$outgoing_light,colours$r,colours$t,colours$horizon),
      lwd=c(1,1,1,1,5),
      legend=c(
          "ingoing light",
          "outgoing light",
          "lines of constant r",
          expression("lines of constant t"["schwarz"]),
          "horizon"
      )
  )

  ## plot the AUT logo:
  if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.8,y=0.08, width=0.1)}  

  git(-0.8,-0.7)

}
