## This file defines thrower() and thrower_asp1(), which is almost the
## same but makes a plot with unit aspect ratio.  


`thrower` <- function(logaxes,legpos="topleft", colours=standard_colours, ...){
    par(lend=1)
    plot(c(0,4),c(0.01,40),
         type='n',xlim=c(0.1,10),ylim=c(0.5,50),
         log=logaxes, xlab='Schwarzschild r',ylab='Schwarzschild t',
         main = "Black hole in Schwarzschild coordinates, with worldlines of free particles", ...)
    
    r_start_outside <- 2
    r_start_inside <- 0.9
    
    t_start_outside <- 0.8
    t_start_inside <- 2.0

    lthick <- 2

    ## trajectory() defined in rwl.R
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.82,sign=-1)       ,type='l',lwd=lthick,col='red'   )
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.82,sign=+1)       ,type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.89,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.88,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.87,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.91,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 1.00,sign=+1,n=1000),type='l',lwd=lthick,col='yellow')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 1.89,sign=+1)       ,type='l',lwd=lthick,col='green' )
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 189 ,sign=+1)       ,type='l',lwd=lthick,col='green' )

    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.30,sign= +1,n=1000),type='l',lwd=lthick,col='blue')
    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.91,sign= +1,n=1000),type='l',lwd=lthick,col='blue')

    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.30,sign= -1,n=1000),type='l',lwd=lthick,col='purple')
    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.91,sign= -1,n=1000),type='l',lwd=lthick,col='purple')

    
    ## event horizon:
    abline(v=1,lwd=6,col=colours$horizon)

    ## singularity:
    if(!isTRUE(grep('x',logaxes))){
      abline(v=0,lwd=4,col=colours$singularity)
    }
    
    ## source of throwing:
    points(r_start_outside,t_start_outside,pch=16)
    points(r_start_inside ,t_start_inside ,pch=16)

    legend(legpos,lty=1,lwd=c(rep(lthick,6),6),
           col=c("red","orange","yellow","green","blue","purple",colours$horizon),
           legend=c("inward throw","outward throw, bounded","critical","unbounded",
                    "interior outward","interior inward","event horizon"))


    ## plot the AUT logo:
    if(!isFALSE(getOption("AUTlogo"))){logo(x=0.83,y=0.04, width=0.1)}  

}

`thrower_asp1`  <- function(colours=standard_colours, ...){
    par(lend=1)
    plot(c(0,4),c(0.01,40),
         type='n',xlim=c(0,5),ylim=c(0,15),asp=1,axes=FALSE,
         xlab='Schwarzschild r',ylab='Schwarzschild t', ...)
    
    
    axis(1,pos=0,at=c(0,5,10))
    axis(2,pos=0)

    r_start_outside <- 2
    r_start_inside <- 0.9

    t_start_outside <- 0.8
    t_start_inside <- 2.0

    lthick <- 2
    
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.82,sign=-1)       ,type='l',lwd=lthick,col='red'   )
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.82,sign=+1)       ,type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.89,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.88,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.87,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 0.91,sign=+1,n=1000),type='l',lwd=lthick,col='orange')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 1.00,sign=+1,n=1000),type='l',lwd=lthick,col='yellow')
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 1.89,sign=+1)       ,type='l',lwd=lthick,col='green' )
    points(trajectory(t0=t_start_outside, r0=r_start_outside, 189 ,sign=+1)       ,type='l',lwd=lthick,col='green' )

    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.30,sign= +1,n=1000),type='l',lwd=lthick,col='blue')
    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.91,sign= +1,n=1000),type='l',lwd=lthick,col='blue')

    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.30,sign= -1,n=1000),type='l',lwd=lthick,col='purple')
    points(trajectory(t0=t_start_inside,r0=r_start_inside,0.91,sign= -1,n=1000),type='l',lwd=lthick,col='purple')

    ## event horizon:
    abline(v=1,lwd=6,col=colours$horizon)

    ## singularity:
    abline(v=0,lwd=4,col=colours$singularity)

    ## source of throwing:
    points(r_start_outside,t_start_outside,pch=16)
    points(r_start_inside, t_start_inside ,pch=16)

    legend(x=5,y=4,lty=1,lwd=c(rep(lthick,6),6),
           col=c("red","orange","yellow","green","blue","purple",colours$horizon),
           legend=c("inward throw","outward throw, bounded","critical","unbounded",
                    "interior inward","interior outward","event horizon"))

    ## plot the AUT logo:
    if(!isFALSE(getOption("AUTlogo"))){logo(x=0.83,y=0.04, width=0.1)}  
}
