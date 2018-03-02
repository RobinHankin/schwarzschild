gullstrand <- function(draw_infalling_drops = FALSE, colours=standard_colours, ...){

cone_function <- function(x,y=1){cone(x,y,-atan(-1-sqrt(1/x)),atan(+1-sqrt(1/x)))}
n <- 4  # size of plot


# plot commands start
plot(NULL,xlim=c(0,n),ylim=c(0,n),ylab=expression(t[d]),xlab='Schwarzschild r',axes=FALSE,asp=1,main='Gullstrand coordinates')

axis(1,pos=0)
axis(2,pos=0)

par(xpd=FALSE)
clip(0,n,0,n)

## constant Schwarzschild r:
abline(v=c(0.5,1,2,3,4),lwd=0.5,col=colours$r)

small <- 1e-4
rin <- seq(from=0,to=1-small,len=1000)
rout <- seq(from=1+small,to=n+1,len=1000)

time <- -20:26

gullstrand    <- function(r,i){
  y <- sqrt(r)
  i - (-2*y + log(abs((y+1)/(y-1))))
}
null_outward <- function(r,i){i+r*(2/sqrt(r)+1) + log(r) +2*log(abs(1/sqrt(r)-1))}
null_inward  <- function(r,i){i+r*(2/sqrt(r)-1) - log(r) -2*log(abs(1/sqrt(r)+1))}

for(i in time){
  jj <- cbind(r=rout,t=null_outward(rout,i))
  points(jj,type='l',lwd=1/2,col=colours$outgoing_light)

  jj <- cbind(r=rin,t=null_outward(rin,i))
  points(jj,type='l',lwd=1/2,col=colours$outgoing_light)

  ## do both inside and outside ingoing in one go:
  jj <- cbind(r=c(rin,rout),t=null_inward(c(rin,rout),i))
  points(jj,type='l',lwd=1/2,col=colours$ingoing_light)

  ## constant Schwarzschild t, outside:
  jj <- cbind(r=rout,t=gullstrand(rout,i))
  points(jj,type='l',lwd=1/2,col=colours$t)

  ## constant Schwarzschild t, inside:
  jj <- cbind(r=rin,t=gullstrand(rin,i))
  points(jj,type='l',lwd=1/2,col=colours$t)

  ## raindrops:
  if(draw_infalling_drops){
    jj <- cbind(c(rin,rout),i-2/3*c(rin,rout)^(3/2))
    points(jj,type='l',lty=2,col=colours$raindrop)
  }
}

#points(cbind(2,0:5),pch=16)  # not right for Gullstrand coords
abline(v=1,lwd=5,col=colours$horizon)
abline(v=0,lwd=5,col=colours$singularity)

cone_function <- function(x,y=1){cone(x,y,-atan(-1-sqrt(1/x)),atan(+1-sqrt(1/x)))}
cone_function(1.001)
cone_function(1.22)
cone_function(1.5)
cone_function(2)
cone_function(2.5)
cone_function(3)
cone_function(3.5)
cone_function(0.5)
cone_function(0.2)

cone_function(0.7,1.7535)


jjang <- 32
text(3.7,0.77,expression('t'['s']==0),col=colours$t,srt=jjang)
text(3.7,1.77,expression('t'['s']==1),col=colours$t,srt=jjang)
text(3.7,2.77,expression('t'['s']==2),col=colours$t,srt=jjang)
text(3.7,3.77,expression('t'['s']==3),col=colours$t,srt=jjang)




par(xpd=TRUE)


if(draw_infalling_drops){
  legend(x=2,y=n-0.35,lty=c(1,1,1,1,2),bg="white",
         col=c(colours$ingoing_light,colours$outgoing_light,
               colours$r,colours$t,colours$raindrop),
         legend=c("ingoing NULL geodesics",
                  "outgoing NULL geodesics",
                  "lines of constant Schwarzschild r",
                  "lines of constant Schwarzschild t",
                  "world lines of infalling drops"
                  ))
} else {
  legend(x=2,y=n-0.4,lty=1,bg="white",
         col=c(colours$ingoing_light,colours$outgoing_light,
               colours$r,colours$t),
         legend=c("ingoing NULL geodesics",
                  "outgoing NULL geodesics",
                  "lines of constant Schwarzschild r",
                  "lines of constant Schwarzschild t"
                  ))
}

}
