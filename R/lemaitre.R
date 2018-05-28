lemaitre <- function(draw_schwarzschild=FALSE, colours=standard_colours, ...){

    op <- par()   # Remember the options and reset at the end of the function
    n <- 2

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
        rhofun <- function(r){ 2/3*sqrt(r)*(r+3)-2*Re(atanh(0i+sqrt(r))) }
        taufun <- function(r){ 2*(sqrt(r)-Re(atanh(0i+sqrt(r)))) }

        rin <- seq(from=0.0000,to=0.999999,len=100)
        rout <- seq(from=1.0001,to=3.9,len=100)

        points(rhofun(rin),taufun(rin)+0,type='l',col=colours$t)
        points(rhofun(rout),taufun(rout)+0,type='l',col=colours$t)
        
        ## lines of constant r:
        for(i in 1:6){
            abline(-i,1,col=colours$r)
        }
        
        text(+1.20,-0.10, 'Schwarzschild t=0, r>1',srt=36,col=colours$t)
        text(-0.75,-1.13, 'Schwarzschild t=0, r<1',srt=50,col=colours$t)
    }

    ## world lines of freely falling raindrops:
    for(i in seq(from=-2,to=3,by=1)){
        segments(x0=i, y0=-n, y1=i, lty=2, col=colours$raindrop)
    }

    ## null geodesics, originating outside event horizon at tau=-2:
    for(jj in -1:5){ null_geodesics(jj,-2,TRUE,TRUE)} # null_geodesics() defined in lemaitre_functions.R
    points(cbind(-1:2,-2),pch=16)
    
    ## outgoing nulls inside 
    null_geodesics(-1.30,-2,FALSE,TRUE) # just barely escapes
    null_geodesics(-1.35,-2,FALSE,TRUE) # just barely trapped
    null_geodesics(-1.50,-2,FALSE,TRUE) # well trapped

    ## lone inward null (this one is on its own, top right):
    null_geodesics(2.50,0,TRUE,FALSE)

    ## singularity:
    segments(-2,-2,2,2,lwd=5,col=colours$singularity)
    text(0,0.15,'singularity',srt=45)

    if(draw_schwarzschild){
        legend(x= -1.8, y=1.8,
               col = c(
                   colours$ingoing_light, colours$outgoing_light, colours$raindrop,
                   colours$r, colours$t,
                   colours$horizon, colours$singularity),
               legend = c(
                   "ingoing light","outgoing light","raindrop worldline",
                   "constant Schwarzschild r","constant Schwarzschild t",
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

    ## plot the AUT logo:
    if(!isFALSE(getOption("AUTlogo"))){logo(x=0.78,y=0.08, width=0.1)}  

  par(family="mono")
git(-2.5,-2.5)
  par(op)  


}
