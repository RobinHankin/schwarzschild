`eddington` <- function(draw_infalling_drops=FALSE, colours=standard_colours, ...){

  n <- 4  # size of plot
  
  plot(NULL,
       xlim=c(0,n),ylim=c(0,n),ylab='',xlab='',
       axes=FALSE,asp=1,
       main="Eddington-Finkelstein coordinates (ingoing)")

  axis(1,pos=0,at=0:4)
  axis(2,pos=0)

  par(xpd=TRUE)
  text(2,-0.5,'Schwarzschild r')
  text(-0.5,2,'tdash',srt=90)

  par(xpd=FALSE)
  clip(0,n,0,n)

  abline(v=c(0.5,1,2,3,4),lwd=0.5,col=colours$r)

  small <- 1e-4
  rin <- seq(from=0,to=1-small,len=1000)
  rout <- seq(from=1+small,to=n,len=1000)

  time <- -20:20
  for(i in time){
    jj <- cbind(rin ,tdash = i+log(abs(rin -1)))
    points(jj,type='l',lwd=1/2,col=colours$t)
    
    jj <- cbind(rout,tdash = i+log(abs(rout-1)))
    points(jj,type='l',lwd=1/2,col=colours$t)
    
    jj <- cbind(rin,tdash = i+rin+2*log(abs(rin-1)))
    points(jj,type='l',col=colours$outgoing_light)
    
    jj <- cbind(rout,tdash = i+rout+2*log(abs(rout-1)))
    points(jj,type='l',col=colours$outgoing_light)

    if(draw_infalling_drops){
      jj <- cbind(rout,tdash = i+raindrop(rout) + log(abs(rout-1)))
      points(jj,type='l',lty=2)
      
      jj <- cbind(rin,tdash = i+raindrop(rin) + log(abs(rin-1)))
      points(jj,type='l',lty=2)
    }
  }



  ## ingoing light makes straight lines; need to separate lower
  ## diagonal lines from upper:
  for(i in seq_len(n)){
    segments(x0=0,y0=i,x1=i,y1=0,col=colours$ingoing_light)
    segments(x0=4,y0=i,x1=4-20,y1=i+20,col=colours$ingoing_light) 
  }
  points(cbind(2,0:5),pch=16)
  abline(v=1,lwd=5,col=colours$horizon)
  abline(v=0,lwd=5,col=colours$singularity)

  ingoing_null_arrow_eddington_ingoing_coords(2.35,3)
  ingoing_null_arrow_eddington_ingoing_coords(0.75,1)
  ingoing_null_arrow_eddington_ingoing_coords(0.55,3)

  outgoing_null_arrow_eddington_ingoing_coords(0.6,3)
  outgoing_null_arrow_eddington_ingoing_coords(0.6,2)
  outgoing_null_arrow_eddington_ingoing_coords(3.4,-3)
  outgoing_null_arrow_eddington_ingoing_coords(1.23,4)

  cf_exterior <- function(x,y=1){cone(x,y,atan(1),atan(1/(1+2/(x-1))))} # lightcone
  cf_exterior(1.001)
  cf_exterior(1.22)
  cf_exterior(1.5)
  cf_exterior(2)
  cf_exterior(2.5)
  cf_exterior(3)
  cf_exterior(3.5)

  cf_interior <- function(x,y=1){cone(x,y,atan(1),atan(1/(1-2/abs(x-1))))}
  cf_interior(0.3)
  cf_interior(0.632)
  cf_interior(0.5, 1.5)
  cf_interior(0.9)
  cf_interior(0.5,0.307)
  cf_interior(0.5,2.113)
  cf_interior(0.15)

  jj_angle <- 21
  text(3.5,0.85,expression('t'['s']==0),col=colours$t,srt=jj_angle)
  text(3.5,1.85,expression('t'['s']==1),col=colours$t,srt=jj_angle)
  text(3.5,2.85,expression('t'['s']==2),col=colours$t,srt=jj_angle)
  text(3.5,3.85,expression('t'['s']==3),col=colours$t,srt=jj_angle)

  par(xpd=TRUE)
  par(lend=1)
  if(draw_infalling_drops){

    legend(
        x=2.1, y=n-1.1,
        bg='white',
        lwd=5, col=c(colours$singularity,colours$horizon),
        legend=c("singularity","horizon")
    )

    legend(
        x=2.1,y=n-0.3,lty=c(1,1,1,1,2),bg='white',
        col = c(colours$ingoing_light,colours$outgoing_light,colours$r,colours$t,colours$raindrop),
        legend=c(
            "ingoing light",
            "outgoing light",
            "lines of constant r",
            expression("lines of constant t"["schwarz"]),
            "infalling raindrops")
    )
  } else {  # do not draw infalling raindrops

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
  }

  ## plot the AUT logo:
  if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.8,y=0.08, width=0.1)}  

  git(-0.8,-0.7)

}
