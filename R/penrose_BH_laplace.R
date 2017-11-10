penrose_BH_laplace <- function(colours=standard_colours, ...){


## This file creates penrose_BH_cauchy.pdf
## plots a Penrose diagram of the whole universe, including a black hole.

## NB cauchy (previously known as 'tan') is the
## only one that looks good---the others
## suck---compare penrose.R where norm looks
## good.


forward <- penrose_transform("laplace",forward=TRUE)

constant_r_exterior <- colours$r
constant_t_exterior <- colours$t

constant_r_interior <- colours$t
constant_t_interior <- colours$r



# set up axes
jj <- c(-0.5,1)
plot(NA,xlim=jj,ylim=jj,asp=1,type='n',axes=FALSE,xlab='',ylab='',main='Penrose diagram, black hole, Laplace transformation')


## First curves of constant Schwarzschild t, exterior
rt_ext <- as.matrix(expand.grid(
    r = c(NA,seq(from=1,to=2,len=300),seq(from=2,to=40,len=1000)),   # the NA is so we can just use plot(...,type='l')
    t = seq(from=-4,to=4,len=9)
))

 ## plot curves of constant Schwarzschild t [ie spacelike curves] on the exterior:
jj <- forward(TX(rt_ext,exterior=TRUE))
points(jj,type='l',lty=1,lwd=0.5,col=colours$t)  # spacelike


## Now curves of constant t (which are spacelike (sic!)) curves on the
## interior:

rt_int <- as.matrix(expand.grid(
    r = c(NA,seq(from=0,to=1,len=3000)),
    t = seq(from=-4,to=4,len=9)
))

points(forward(TX(rt_int,exterior=FALSE)),type='l',lty=1,lwd=0.5,col=colours$t)


r_values <- c(1.05, 1.1, 1.2, 1.3, 1.5, 2, 2.3)

rt_exterior <- as.matrix(expand.grid(
    t = c(NA,seq(from=-10,to=10,len=1000)),
    r = r_values
))[,2:1]

points(forward(TX(rt_exterior,exterior=TRUE)),type='l',lty=1,lwd=0.5,col=constant_t_interior)

r_values_inside <- c(0.95, 0.8, 0.6, 0.4,0.1,0.01,0)
 ## plot curves of constant Schwarzschild r on the interior:
# Now timelike curves, interior
rt_int <- as.matrix(expand.grid(
    t = c(NA,seq(from=-10,to=10,len=1000)),
    r = r_values_inside 
))[,2:1]

points(forward(TX(rt_int,exterior=FALSE)),type='l',lty=1,lwd=0.5,col=constant_t_interior)



## leftward pointing light curves:
jj <- forward(TX(cbind(r_values,0),exterior=TRUE))[,1]


## Each leftward pointing light curve ends on the singularity;
## variable 'extra_len makes sure the light curvs actually end on the singularity:

extra_len <- c(0.12,0.13,0.13, 0.13, 0.13, 0.12, 0.1)
 ## one per light curve

for(i in seq_along(jj)){
    xstart <- jj[i]
    delta <- extra_len[i]
    segments(x0  = xstart,  # ingoing
             y0  = 0,
             x1  = -0.5+xstart-delta,
             y1  = +0.5       +delta,
             col = colours$ingoing_light
             )
    ## Now plot outgoing
  segments(x0=xstart,y0=0,x1=(xstart+1)/2,y1=(1-xstart)/2,col=colours$outgoing_light)
}

points(cbind(jj,0),pch=16)


## draw the singularity:
last <- max(which(is.na(rowSums(rt_int))))
jj <- rt_int[(last+1):nrow(rt_int),]
points(forward(TX(jj,exterior=FALSE)),type='l',lty=1,lwd=5,col=colours$singularity)


## draw the boundary of the universe
segments(x0=0.5,y0=0.5,x1=1,y1=0,lwd=1,col=colours$singularity)
segments(x0=1,y0=0,x1=0.5,y1=-0.5,lwd=1,col=colours$singularity)


## last thing, draw the horizons

size <- 33
## do the horizons last:
segments(x0=-0.5,y0=0.5,x1=0.5,y1=-0.5, col=colours$horizon,lwd=5)
segments(x0=-0,y0=0,x1=0.5,y1=0.5, col=colours$horizon,lwd=5)

## some cones

cone(0.1,0.2,pi/4,pi/4,0.05)
cone(0.5,0.2,pi/4,pi/4,0.05)
cone(0.5,-0.25,pi/4,pi/4,0.05)
cone(-0.2,0.4,pi/4,pi/4,0.05)

## label some constant-r [timelike] curves on the exterior
text(0.31,-0.12,labels=paste("r = ",r_values[1],sep=""),col=colours$r,srt=-80)
text(0.41,-0.10,labels=paste("r = ",r_values[2],sep=""),col=colours$r,srt=-85)
text(0.53,-0.10,labels=paste("r = ",r_values[3],sep=""),col=colours$r,srt=-90)
text(0.63,-0.10,labels=paste("r = ",r_values[4],sep=""),col=colours$r,srt=-90)
text(0.74,-0.10,labels=paste("r = ",r_values[5],sep=""),col=colours$r,srt=-90)
#text(0.86,-0.1,labels=paste("r = ",r_values[6],sep=""),col=colours$t,srt=83)
#text(0.99,-0.06,labels=paste("r = ",r_values[7],sep=""),col=colours$t,srt=53)

## label some constant-t [spacelike] curves on the exterior
text(0.23,-0.024,labels="t=0",col=colours$t,srt=0  )
text(0.20,-0.06,labels="t=-1",col=colours$t,srt=-20)
text(0.22,-0.13,labels="t=-2",col=colours$t,srt=-30)

## now label some constant t [spacelike] curves on the interior
text(-0.12,0.2,labels="t=-2",col=colours$t,srt=-58)
text(-0.044,0.15,labels="t=-1",col=colours$t,srt=-70)
text(0.02,0.12,labels="t=0",col=colours$t,srt=-90)
text(0.12,0.37,labels="t=1",col=colours$t,srt=70)
text(0.19,0.31,labels="t=2",col=colours$t,srt=60)

## label some constant r [timelike] curves on the interior
text(-0.22,0.33,labels=paste("r = ",r_values_inside[1],sep=""),col=colours$r,srt=-28)
text(-0.26,0.49,labels=paste("r = ",r_values_inside[2],sep=""),col=colours$r,srt=-10)
text(-0.23,0.56,labels=paste("r = ",r_values_inside[3],sep=""),col=colours$r,srt=-6)

legend(x=-0.5,y=-0.2,lty=1,lwd=c(1,1,0.5,0.5,5,5),
       col=c(colours$ingoing_light,colours$outgoing_light,colours$t,colours$r,colours$singularity,colours$horizon),
       legend=c("ingoing light","outgoing light","constant Schwarzschild t","constant Schwarzschild r","singularity","horizon"))


}
