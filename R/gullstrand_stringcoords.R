gullstrand_stringcoords <- function(draw_infalling_drops = TRUE, colours=standard_colours, ...){

    par(lend=1)
    n <- 4  # size of plot

    plot(NULL,xlim=c(0,n),ylim=c(0,n),ylab=expression(t[d]),xlab='string coordinate',axes=FALSE,asp=1,main='Gullstrand/string coordinates')

    axis(1,pos=0)
    axis(2,pos=0)

    par(xpd=FALSE)
    clip(0,n,0,n)

    ## constant Schwarzschild r:
    abline(v=c(0.5,1,2,3,4),lwd=0.5,col=colours$r)

    small <- 1e-5
    rout <- seq(from=1+small,to=n+1,len=10000)
    rin <- NULL # in the interest of small changes from gullstrand()


    togullstrand <- function(r){
        y <- sqrt(r)
        -(-2*y + log(abs((y+1)/(y-1))))
    }

    null_outward <- function(r){r*(2/sqrt(r)+1) + log(r) +2*log(abs(1/sqrt(r)-1))}
    null_inward  <- function(r){r*(2/sqrt(r)-1) - log(r) -2*log(abs(1/sqrt(r)+1))}

    points_outward <- cbind(u=u1(rout),t=null_outward(rout))
    points_inward  <- cbind(u=u1(rout),t=null_inward(rout))

    points_outward <- shifter(points_outward)
    points_inward  <- shifter(points_inward)
    
    ## constant Schwarzschild t, outside:

    xcoord <- u1(rout)
    tcoord <- togullstrand(rout)
    val <- approxfun(xcoord,tcoord)(1)
    tcoord <- tcoord - val

    points_const_t <- cbind(u=xcoord,t=tcoord)

    points_drops <- shifter(cbind(u1(rout),-2/3*c(rin,rout)^(3/2)))





    for(i in 0:5){
        jj_inward <- points_inward
        jj_inward[,2] <- jj_inward[,2] + i
        points(jj_inward,type='l',lwd=1/2,col=colours$ingoing_light)
    }

    for(i in -9:24){
        jj_outward <- points_outward
        jj_outward[,2] <- jj_outward[,2] + i
        points(jj_outward,type='l',lwd=1/2,col=colours$outgoing_light)
    }

    for(i in -9:5){
        jj_const_t <- points_const_t
        jj_const_t[,2] <- jj_const_t[,2] + i
        points(jj_const_t,type='l',lwd=1/2,col=colours$t)
    }
    ## raindrops:
    if(draw_infalling_drops){
        for(i in 0:9){
            jj_drops <- points_drops
            jj_drops[,2] <- jj_drops[,2] + i
            points(jj_drops,type='l',lty=2,col=colours$raindrop)
        }
    }
    
    abline(v=0,lwd=5,col=colours$horizon)
    par(xpd=NA)
    par(xpd=TRUE)

    jjang <- 32
    text(3.2,0.68,expression('t'['s']==1),col=colours$t,srt=jjang)
    text(3.2,1.68,expression('t'['s']==2),col=colours$t,srt=jjang)
    text(3.2,2.68,expression('t'['s']==3),col=colours$t,srt=jjang)
    text(3.2,3.68,expression('t'['s']==4),col=colours$t,srt=jjang)

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

    points(cbind(1,0:4),pch=16)
    
    ## plot the AUT logo:
    if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.8,y=0.06, width=0.1)}  
    
    git(-0.8,-0.7)
    
}
