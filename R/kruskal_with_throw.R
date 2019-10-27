`kruskal_with_throw` <- function(draw_schwarzschild = FALSE, label_curves = FALSE, colours=standard_colours){

  ## set the two arguments to TRUE to give more detail (but these give a
  ## very cluttered diagram)

  ## NB: everything here is (space,time); not (time,space).  This is
  ## because it is easier to plot in R


  ingoing_null <- colours$ingoing_light
  outgoing_null <- colours$outgoing_light

  constant_r_exterior <- colours$r
  constant_t_exterior <- colours$t

  constant_r_interior <- colours$t
  constant_t_interior <- colours$r

  ## points from which to emit a ray of light (and draw spacelike curves
  ## of constant Schwarzschild r from):
  r_emitting <-  seq(from=1.05,len=4,to=2)

  size <- 2
  par(pty='s')
  plot(NULL,type='n',asp=1,xlim=c(-size,size),ylim=c(-size,size),axes=FALSE,xlab="",ylab="",main="Kruskal-Szekeres coordinates")
  clip(-2,2,-2,2)

  par(xpd=FALSE)


  if(draw_schwarzschild){
    ## First spacelike curves, exterior:
    rt_ext <- as.matrix(expand.grid(
        r = c(NA,seq(from=1,to=40,len=100)),   # the NA is so we can just use plot(...,type='l')
        t = seq(from=-4,to=4,len=9)
    ))

    text(0,-1.9,"NB: lines of constant Schwarzchild r are timelike outside and spacelike inside")
    ## plot curves of constant Schwarzschild r on the exterior:
    jj <- TX(rt_ext,exterior=TRUE)
    points(jj,type='l',lty=1,lwd=0.5,col=constant_r_exterior)  # spacelike

    ## Now curves of constant Schwarzschild t, exterior:
    rt_ext <- as.matrix(expand.grid(
        t = c(NA,seq(from=-4,to=4,len=1000)),
        r = r_emitting
    ))[,2:1]
    points(TX(rt_ext,exterior=TRUE),type='l',lwd=0.5,lty=1,col=constant_t_exterior)  # timelike


    ## Now spacelike (sic!) curves on the interior:

    rt_int <- as.matrix(expand.grid(
        r = c(NA,seq(from=0,to=1,len=300)),
        t = seq(from=-4,to=4,len=9)
    ))

    points(TX(rt_int,exterior=FALSE),type='l',lty=1,lwd=0.5,col = constant_t_interior)

    ## Now timelike curves, interior
    rt_int <- as.matrix(expand.grid(
        t = c(NA,seq(from=-4,to=4,len=1000)),
        r = c(0.95, 0.8, 0.6, 0.4,0.1)
    ))[,2:1]

    points(TX(rt_int,exterior=FALSE),type='l',lty=1,lwd=0.5,col=constant_r_interior)
  }


  ## Now the antisingularity; the other square root:
  ## jj[,2]<- -jj[,2]  
  ## points(jj      ,type='l',col='black',lwd=8)


  ## light curves:

  if(FALSE){
    l <- 10
    for(i in r_emitting){
      X <-  sqrt(i-1)*exp(i/2)  # KS coordinate 'X'
      segments(x0=X,y0=0,  # start point
               x1 = 0.5*(X-1/X), y1=-0.5*(-X-1/X), col=ingoing_null)  # ingoing
      segments(x0=X,y0=0,  # start point
               x1=X+l,y1=X+10,col=outgoing_null)  
    }
  }

  if(label_curves){
    ## label some timelike curves on the exterior
    text(0.53,-0.22,labels=paste("r = ",r_emitting[1],sep=""),col=colours$t,srt=-60)
    text(1.33,-0.30,labels=paste("r = ",round(r_emitting[2],2),sep=""),col=colours$t,srt=-80)
    text(2.05,-0.30,labels=paste("r = ",round(r_emitting[3],2),sep=""),col=colours$t,srt=-82)

    ## label some spacelike curves on the exterior
    text(0.7,0.06,labels=paste("t = ",0,sep=""),col=colours$r,srt=0)
    text(0.8,-0.3,labels=paste("t = ",-1,sep=""),col=colours$r,srt=-25)
    text(1.1,-0.75,labels=paste("t = ",-2,sep=""),col=colours$r,srt=-35)
    text(1.7,-1.45,labels=paste("t = ",-3,sep=""),col=colours$r,srt=-40)
    text(1,0.53,labels=paste("t = ",1,sep=""),col=colours$r,srt=25)
    text(1.1,0.9,labels=paste("t = ",2,sep=""),col=colours$r,srt=35)


    ## label some spacelike curves on the interior (constant t [sic])
    text(0.05,0.5,labels=paste("t = ",0,sep=""),col=colours$r,srt=90)
    text(0.22,0.6,labels=paste("t = ",1,sep=""),col=colours$r,srt=64)
    text(0.60,0.9,labels=paste("t = ",2,sep=""),col=colours$r,srt=52)

    ## label some timelike curves on the interior (constant t [sic])
    text(-0.39,0.6,labels=paste("r = ",0.95,sep=""),col=colours$t,srt=-35)
    text(-0.1,0.74,labels=paste("r = ",0.8,sep=""),col=colours$t,srt=-10)
    text(-0.4,0.9,labels=paste("r = ",0.6,sep=""),col=colours$t,srt=-22)
  }


  par(lend=1)

  legend(x=-2,y=0.6,lty=1,lwd=5, col=c(colours$horizon, colours$singularity),
         legend=c("horizon","singularity")
         )

  legend(x=-2,y=0,lty=1,lwd=1,
         col=c(colours$captured, colours$critical, colours$escaping),
         legend=c('captured world lines','critical case','escaping world lines')
         )

  if(FALSE){
    legend(x=-2,y=-0.6,lty=1,lwd=0.5,
           col=c(colours$r,colours$t),
           legend=c("lines of constant Schwarzschild r",
                    "lines of constant Schwarzschild t")
           )
  }
  ## do some light cones:
  jj <-TX(cbind(r_emitting,0),exterior=TRUE)
  cone(jj[1,1],jj[1,2],pi/4,pi/4,size=0.1)
  cone(1.9,1.25,pi/4,pi/4,size=0.1)
  cone(1.7,-0.4,pi/4,pi/4,size=0.1)
  cone(-0.75,0.9,pi/4,pi/4,size=0.1)


  lthick <- 1

  f <- function(epsilon,r0,t0,sign, doplot=TRUE, ...){
    ## find the world-line in Schwarzschild coords:
    jj <- trajectory(t0=t0, r0=r0, epsilon=epsilon, sign=sign,n=20000)

    tx <- TX(jj,r0>1)

    if(doplot){
      points(tx,type='l',lwd=lthick,...)
    }
    return(tx)
  }

  f(1.00, 1.37, 0, sign=-1, col=colours$captured)
  f(1.00, 0.999, 6.47, sign=-1, col=colours$captured)

  f(0.80, 1.37, 0, sign=-1, col=colours$captured)
  f(0.80, 0.999, 6.62, sign=-1,col=colours$captured)

  f(0.70, 1.37, 0, sign=-1, col=colours$captured)
  f(0.70, 0.999, 6.77, sign=-1,col=colours$captured)

  f(0.62, 1.37, 0, sign=-1, col=colours$captured)
  f(0.62, 0.999, 7.00, sign=-1,col=colours$captured)

  f(0.57, 1.37, 0, sign=-1, col=colours$captured)
  f(0.57, 0.999, 7.275, sign=-1,col=colours$captured)

  f(0.52, 1.37, 0, sign=-1, col=colours$captured)
  f(0.52, 0.999, 8.3, sign=-1, col=colours$captured)

  f(0.55, 1.37, 0, sign=+1,col=colours$critical)


                                        #world-line, inside the event horizon, that terminates before the
                                        #singularity:
  f(0.55, 1.24, -1, sign=-1,col=colours$captured)
  jj <- f(0.55, 0.999, 5.27, sign=-1,doplot=FALSE)
  points(jj[1:2000,],type='l',lwd=lthick,col=colours$captured)

  jjj <- RT(jj)

  r_explosion <- jjj[2000,1]
  t_explosion <- jjj[2000,2]

  f(epsilon=0.01, r_explosion,t_explosion,sign=1,col=colours$captured)
  f(epsilon=0.20, r_explosion,t_explosion,sign=1,col=colours$captured)
  f(epsilon=0.30, r_explosion,t_explosion,sign=1,col=colours$captured)
  f(epsilon=0.40, r_explosion,t_explosion,sign=1,col=colours$captured)

  f(epsilon=0.20, r_explosion,t_explosion,sign=-1,col=colours$captured)
  f(epsilon=0.30, r_explosion,t_explosion,sign=-1,col=colours$captured)
  f(epsilon=0.40, r_explosion,t_explosion,sign=-1,col=colours$captured)
  f(epsilon=3.40, r_explosion,t_explosion,sign=-1,col=colours$captured)

  f(9.72, 1.37, 0, sign=+1,col=colours$escaping)
  f(1.00, 1.37, 0, sign=+1,col=colours$escaping)
  f(0.62, 1.37, 0, sign=+1,col=colours$escaping)

  ## singularity:
  rt_sing <- cbind(r=0,t=seq(from=-4,to=4,len=1000))
  jj <- TX(rt_sing,exterior=FALSE)
  points(jj,type='l',col=colours$singularity,lwd=8)

  polygon(jj,col=colours$singularity_interior)

  ## describe r<0 region:
  text(0,1.6,expression(r<0),cex=1.6)


  size <- 33
  ## do the horizons last:
  segments(0,0,size,size,col=colours$horizon,lwd=5)
  segments(size,-size,-size,size,col=colours$horizon,lwd=5)

  ## plot the AUT logo:
  if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.84,y=0.08, width=0.1)}  

  par(family="mono")
  git(-2,-2)

}
