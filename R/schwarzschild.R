schwarzschild <- function(draw_infalling_drops=FALSE, colours=standard_colours,...){

n <- 4  # size of plot


par(xpd=TRUE)
clip(0,n,0,n)

plot(0:n,0:n,xlim=c(0,n),ylim=c(0,n),asp=1,type='n',xlab='',ylab='',axes=FALSE)
axis(1,pos=0,at=0:n)
axis(2,pos=0,at=0:n)

par(xpd=TRUE)
text(2,-0.5,'Schwarzschild r')
text(-0.5,2,'Schwarzschild t',srt=90)

for(i in 0:3){
  segments(x0=i,y0=0,y1=n,col=colours$r,lwd=0.5,lty=1)  # horizontal lines; lines of constant r
  segments(x0=0,x1=n,y0=i,col=colours$t,lwd=0.5,lty=1)  # vertical lines; lines of constant t
}

par(xpd=FALSE)
tee <- seq(from=-10, to=10, len=10000)

jj <- ingoing(tee,0,0)
wanted <- !is.na(jj)
t_ingoing_inside <- c(0,tee[wanted])
r_ingoing_inside <- c(0,jj[wanted])

jj <- ingoing(tee,2,0)
t_ingoing_outside <- tee
r_ingoing_outside <- jj


jj <- outgoing(tee,2,0)
t_outgoing_outside <- tee
r_outgoing_outside <- jj

jj <- outgoing(tee,0,0)

wanted <- !is.na(jj)
## NB these lines go backwards, so zero is at the end (not at the
## beginning as it was for t_ingoing_inside):
t_outgoing_inside <- c(tee[wanted],0) 
r_outgoing_inside <- c(jj[wanted],0)

offset <- -6:9

for(i in offset){ points(r_ingoing_outside,t_ingoing_outside+i, type='l',col=colours$ingoing_light) }
for(i in offset){ points(r_outgoing_outside,t_outgoing_outside+i, type='l',col=colours$outgoing_light) }
for(i in offset){ points(r_ingoing_inside ,t_ingoing_inside +i, type='l',col=colours$ingoing_light) }
for(i in offset){ points(r_outgoing_inside ,t_outgoing_inside +i, type='l',col=colours$outgoing_light) }


r_inside <- seq(from=0.0001,to=0.999,len=1000)
r_outside <- seq(from=1.0001,to=n,len=1000)


if(draw_infalling_drops){

  for(i in offset){points(r_inside  ,raindrop(r_inside)+i, type='l',lty=2) }
  for(i in offset){points(r_outside ,raindrop(r_outside)-raindrop(2)+i, type='l',lty=2) }

  raindrop_arrow(0.4,-0.01,2)  # negative because inside SR
  raindrop_arrow(0.6,-0.01,2)  # negative because inside SR
  raindrop_arrow(0.8,-0.01,2)  # negative because inside SR

  raindrop_arrow(1.5,-0.001,1-raindrop(2))  # "1-" because object passes through (2,1)
  raindrop_arrow(1.5,-0.001,2-raindrop(2))  # "1-" because object passes through (2,1)
}


## draw arrows on some null curves:

outgoing_null_arrow(0.56,2,colours=colours)
outgoing_null_arrow(0.40,2,colours=colours)

outgoing_null_arrow(0.56,3,colours=colours)
outgoing_null_arrow(0.40,3,colours=colours)

outgoing_null_arrow(2.50, 0,colours=colours)
outgoing_null_arrow(1.50, 0,colours=colours)
outgoing_null_arrow(1.10, 5,colours=colours)
outgoing_null_arrow(3.76,-1,colours=colours)

ingoing_null_arrow(0.56,1,colours=colours)
ingoing_null_arrow(0.40,1,colours=colours)

ingoing_null_arrow(0.56,2,colours=colours)
ingoing_null_arrow(0.40,2,colours=colours)

ingoing_null_arrow(1.81,3,colours=colours)
ingoing_null_arrow(1.45,3,colours=colours)
ingoing_null_arrow(3.76,8,colours=colours)


par(xpd=FALSE)
abline(v=1,lwd=4,col=colours$horizon)

polygon(x=c(0,0,n+1,n+1),y=c(n,n+1,n+1,n),border=NA,lwd=7,col='white')

#  points(r_ingoing_inside ,t_ingoing_inside +i+0.5, type='l',col=colours$ingoing_light,lwd=0.3)
#  points(r_ingoing_outside,t_ingoing_outside+i+0.5, type='l',col=colours$ingoing_light,lwd=0.3)

#  points(r_outgoing_inside ,t_outgoing_inside +i+0.5, type='l',col=colours$outgoing_light,lwd=0.3)
#  points(r_outgoing_outside,t_outgoing_outside+i+0.5, type='l',col=colours$outgoing_light,lwd=0.3)

par(xpd=TRUE)
if(draw_infalling_drops){
  legend(x=2.5,y=n+0.6,lty=c(1,1,2),
         col=c(colours$r, colours$t, colours$raindrop),
         legend=c("lines of constant r",
                  "lines of constant t",
                  "world lines of objects in freefall"
                  ))
} else {
    legend(x=2.5, y=n+0.6, lty=1,
           col=c(colours$r,colours$t),
           legend=c("lines of constant r","lines of constant t")
           )
}

    legend(x=0,y=n+0.6,lty=1,
           col=c(colours$ingoing_light,colours$outgoing_light),
           legend=c("ingoing NULL geodesics","outgoing NULL geodesics"))

lightcone(3.0,1,size=0.1)
lightcone(2.0,1,size=0.1)
lightcone(1.5,1,size=0.1)
lightcone(0.8,1,size=0.1)
lightcone(0.5,1,size=0.1)
lightcone(0.2,1,size=0.1)

points(cbind(2,offset),pch=16)

}
