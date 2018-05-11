penrose_norm <- function(colours=standard_colours, ...){

## plots a Penrose diagram of the whole universe.  We can choose colours

penrose <- penrose_transform("norm")

## NB: norm looks good and tan sucks; cf penrose_black_hole


# set up axes
jj <- c(-1,1)
plot(jj,jj,asp=1,type='n',axes=FALSE,xlab='',ylab='',main='normal (Gaussian) transformation')

## plot 'boundary' of universe

segments(x0=c(-1,0,1,0),
         y0=c(0,1,0,-1),
         x1=c(0,1,0,-1),
         y1=c(1,0,-1,0),
         col='gray'
         )

## some annotation
text(0,1.03,'distant future')
text(0,-1.03,'distant past')
text(-1.1,0,'far\n away\n left')
text(1.1,0,'far\naway\nright')

o <- seq(from=0,to=1,len=5)

## now a key:
for(i in o/10 +0.95){segments(x0=-1.1,x1=-1,y0=i,y1=i,col=colours$spacelike_curve)}
text(-1,1,"spacelike curves",pos=4)

for(i in o/10 - 1){segments(x0=i-0.1,x1=i-0.1,y0=0.75,y1=0.85,col=colours$timelike_curve)}
text(-1,0.8,"timelike curves",pos=4)

for(i in o/10/sqrt(2)){segments(x0=-1-.05-i,x1=-1.1+0.1+0.05/2-i,y0=0.55+i,y1=0.6+0.05/2+i,col=colours$photon)}
text(-1,0.6,"null curves",pos=4)

## first timelike vectors:

jj <- c(-1000,seq(from= -20,to=20, len=1000),1000)

thingvec <- seq(from=-18,to=18,by=1)

for(i in thingvec){
  ## first timelike vectors:
  xt <- cbind(x=i,t=jj)
  points(penrose(xt),type='l',col=colours$timelike_curve)

  ## now spacelike vectors
  xt <- cbind(x=jj,t=i)
  points(penrose(xt),type='l',col=colours$spacelike_curve)
}

if(FALSE){
# plot some null curves, all emitted at the same time:

start_time <- -2
end_time <- 100

for(i in thingvec){
  points( penrose(
      cbind(x=c(i,i+end_time),
            t=c(start_time,  start_time + end_time)
            )
      
  ),col=colours$photon,type='l')
}

}


# plot some null curves, all emitted at the same time:
start_x <- -2
end_time <- 100

for(i in thingvec){
  points(penrose(
      cbind(x=c(start_x,start_x+end_time),
            t=c(i, i+end_time)
            )
      
  ), col=colours$photon,type='l')
}
## plot the AUT logo:
    if(!isFALSE(getOption("AUTlogo"))){logo(x=0.85,y=0.20, width=0.1)}  
}
