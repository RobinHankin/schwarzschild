gullstrand <- function(draw_infalling_drops = FALSE, colours=standard_colours, ...){

    par(lend=1)
    n <- 4  # size of plot

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

    togullstrand <- function(r,i){
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
        jj <- cbind(r=rout,t=togullstrand(rout,i))
        points(jj,type='l',lwd=1/2,col=colours$t)

        ## constant Schwarzschild t, inside:
        jj <- cbind(r=rin,t=togullstrand(rin,i))
        points(jj,type='l',lwd=1/2,col=colours$t)

        ## raindrops:
        if(draw_infalling_drops){
            jj <- cbind(c(rin,rout),i-2/3*c(rin,rout)^(3/2))
            points(jj,type='l',lty=2,col=colours$raindrop)
        }
    } # for(i in time) loop closes

    abline(v=1,lwd=5,col=colours$horizon)
    par(xpd=NA)
    segments(x0=0,y0=0,y1=n,lwd=5,col=colours$singularity)
    par(xpd=TRUE)
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

    if(draw_infalling_drops){
      legend(x=2.1,y=n-0.35,lty=c(1,1,1,1,2), lwd=c(0.5,0.5,0.5,0.5,1),
             bg="white",
               col=c(
                   colours$ingoing_light,
                   colours$outgoing_light,
                   colours$r,
                   colours$t,
                   colours$raindrop),
               legend=c("ingoing light",
                        "outgoing light",
                        expression("lines of constant r"["schwarz"]),
                        expression("lines of constant t"["schwarz"]),
                        "world lines of infalling drops"
                        ))
    } else {
        legend(x=2.1,y=n-0.4,lty=1,bg="white",lwd=c(0.5,0.5,0.5,0.5,1),
               col=c(colours$ingoing_light,
                     colours$outgoing_light,
                     colours$r,
                     colours$t),
               legend=c("ingoing light",
                        "outgoing light",
                        expression("lines of constant r"["schwarz"]),
                        expression("lines of constant t"["schwarz"])
                        ))
    }

    ## plot the AUT logo:
    if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.8,y=0.06, width=0.1)}  
    
    git(-0.8,-0.7)
    
}
