eddington_outgoing <-  function(colours=standard_colours, ...){

n <- 4  # size of plot

## plot commands start
plot(NULL,xlim=c(0,n),ylim=c(0,n),ylab='tdash',xlab='Schwarzschild r',axes=FALSE,asp=1)

axis(1,pos=0)
axis(2,pos=0)

par(xpd=FALSE)
clip(0,n,0,n)


abline(v=c(0.5,1,2,3,4),lwd=0.5,col=colours$r)

small <- 1e-4
rin <- seq(from=0,to=1-small,len=1000)
rout <- seq(from=1+small,to=n,len=1000)

time <- -20:20
for(i in time){
  jj <- cbind(rin ,tdash = i-log(abs(rin -1)))
  points(jj,type='l',lwd=1/2,col=colours$t)
  
  jj <- cbind(rout,tdash = i-log(abs(rout-1)))
  points(jj,type='l',lwd=1/2,col=colours$t)
  
  jj <- cbind(rin,tdash = i-rin-2*log(abs(rin-1)))
  points(jj,type='l',col=colours$ingoing_light)
  
  jj <- cbind(rout,tdash = i-rout-2*log(abs(rout-1)))
  points(jj,type='l',col=colours$ingoing_light)
}

## outgoing light
for(i in seq_len(n)){
  segments(x0=0,y0=i,x1=4-i,y1=4,col=colours$outgoing_light)
  segments(x0=i,y0=0,x1=4,y1=4-i,col=colours$outgoing_light)
}

## outgoing light from (0,0):
  segments(x0=0,y0=0,x1=4,y1=4,col=colours$outgoing_light)

points(cbind(2,0:5),pch=16)
abline(v=1,lwd=5,col=colours$horizon)
abline(v=0,lwd=5,col=colours$singularity)



cf_exterior <- function(x,y=1){cone(x,y,atan(1/(1+2/(x-1))),atan(1))}
cf_exterior(1.001)
cf_exterior(1.22)
cf_exterior(1.5)
cf_exterior(2)
cf_exterior(2.5)
cf_exterior(3)


cf_interior <- function(x,y=1){cone(x,y,atan(1/(1-2/abs(x-1))),atan(1))}
cf_interior(0.3)
cf_interior(0.632)
cf_interior(0.9)
cf_interior(0.15)

cf_interior(0.5, 1.5)
cf_interior(0.5,0.69)
cf_interior(0.5,1.88)


jjang <- -21

text(3.5,0.17,expression('t'['s']==0),col=colours$t,srt=jjang)
text(3.5,1.17,expression('t'['s']==1),col=colours$t,srt=jjang)
text(3.5,2.17,expression('t'['s']==2),col=colours$t,srt=jjang)
text(3.5,3.17,expression('t'['s']==3),col=colours$t,srt=jjang)


par(xpd=TRUE)
legend(x=0,y=n+0.6,lty=1,col=c(colours$ingoing_light,colours$outgoing_light),legend=c("ingoing NULL geodesics","outgoing NULL geodesics"))
legend(x=2,y=n+0.6,lty=1,col=c(colours$r,colours$t),legend=c("lines of constant Schwarzschild r","lines of constant Schwarzschild t"))


}
