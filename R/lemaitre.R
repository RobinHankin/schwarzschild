lemaitre <- function(draw_schwarzschild =FALSE, colours=standard_colours, ...){

taufun <- function(r){  2*(sqrt(r)-Re(atanh(0i+sqrt(r)))) }
rhofun <- function(r){  2/3*sqrt(r)*(r+3)-2*Re(atanh(0i+sqrt(r))) }

n <- 2

rin <- seq(from=0.0000,to=0.999999,len=100)
rout <- seq(from=1.0001,to=3.9,len=100)

par(pty='s')
par(xpd=FALSE)
par(lend=1)

plot(NA,xlim=c(-n,n),ylim=c(-n,n),type='n',xlab='rho',ylab='tau',asp=1,axes=FALSE, main='Lemaitre coordinates')
axis(1,pos=-n)
axis(2,pos=-n)


## singularity interior:
polygon(x=c(-n,n,-n),y=c(-n,n,n),col=colours$singularity_interior,border=NA)


## event horizon:
segments(-4/3,-2,3,7/3,lwd=5,col=colours$horizon)
text(1.6,1.05,'event horizon, r=1',srt=45,col=colours$horizon)

if(draw_schwarzschild){
    
    ## lines of constant t:
    points(rhofun(rin),taufun(rin)+0,type='l',col=colours$t)
    points(rhofun(rout),taufun(rout)+0,type='l',col=colours$t)
    
    ## lines of constant r:
    for(i in 1:6){
        abline(-i,1,col=colours$r)
    }
    
    text(+2.2,+0.78,'Schwarzschild t=0, r>1',srt=28,col=colours$t)
    text(-0.8,-1.10,'Schwarzschild t=0, r<1',srt=50,col=colours$r)
}

## world lines of freely falling raindrops:
for(i in seq(from=-2,to=3,by=1)){
    segments(x0=i, y0=-n, y1=i, lty=2)
    }

## null geodesics, outside event horizon:
for(jj in -1:5){ null_geodesics(jj,-2)}

## null geodesics inside event horizon:
#null_geodesics(1,0.5)
#null_geodesics(1,0)

## outgoing nulls inside 
null_geodesics(-1.30,-2,FALSE,TRUE) # just barely escapes
null_geodesics(-1.35,-2,FALSE,TRUE) # just barely trapped
null_geodesics(-1.50,-2,FALSE,TRUE) # well trapped

## lone inward null:

null_geodesics(2.50,0,TRUE,FALSE)  # well trapped

## singularity:
segments(-2,-2,2,2,lwd=5,col=colours$singularity)
text(0,0.15,'singularity',srt=45)

if(draw_schwarzschild){
    legend(x= -1.5, y=1.5,
           col = c(
               colours$ingoing_light, colours$outgoing_light, colours$raindrop,
               colours$r, colours$t,
               colours$horizon, colours$singularity),
           legend = c(
               "ingoing light","outgoing light","raindrop worldline",
               "constant Schwarzschild r","constant Schwartzschild t",
               "event horizon", "singularity"),
           lty=c(1,1,2,1,1,1,1),
           lwd=c(1,1,1,1,1,1,5),
           bg='white')
} else {
    legend(x= -1.5, y=1.5,
           col = c(
               colours$ingoing_light, colours$outgoing_light, colours$raindrop,
               colours$horizon, colours$singularity),
           legend = c(
               "ingoing light","outgoing light","raindrop worldline",
               "event horizon", "singularity"
           ),
           lty=c(1,1,2,1,1),
           lwd=c(1,1,1,5,5),
           bg='white')
}
}
