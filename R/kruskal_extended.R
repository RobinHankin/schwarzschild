kruskal_extended <- function(colours = standard_colours, ...){

    ## NB: everything here is (space,time).  This is because it is easier to plot in R

    constant_r_exterior <- colours$r
    constant_t_exterior <- colours$t

    constant_r_interior <- colours$t
    constant_t_interior <- colours$r

    ## points from which to emit a ray of light (and draw spacelike
    ## curves of constant Schwarzchild r from):
    
    r_emitting <-  seq(from=1.05,len=4,to=2)

    size <- 2
    par(pty='m')


    ## plot commands start
    plot(NULL, type='n', asp=1,
         xlim=c(-size,size),ylim=c(-size,size),
         axes=FALSE, xlab="", ylab="",
         main="Extended Kruskal-Szekeres coordinates")
    clip(-2,2,-2,2)
    par(xpd=FALSE)

    ## First spacelike curves, exterior:
    rt_ext <- as.matrix(expand.grid(
        r = c(NA,seq(from=1,to=40,len=100)),   # the NA is so we can just use plot(...,type='l')
        t = seq(from=-4,to=4,len=9)
    ))

    ## plot curves of constant Schwarzschild r on the exterior:
    jj <- TX(rt_ext,exterior=TRUE)
    points(jj,type='l',lty=1,lwd=0.5,col=constant_r_exterior)  # spacelike

    ## Antiuniverse:
    jj[,1] <- -jj[,1]
    points(jj,type='l',lty=1,lwd=0.5,col=constant_r_exterior)  # spacelike

    text(0,-2,"NB: lines of constant Schwarzchild r are timelike outside and spacelike inside")

    ## Now curves of constant Schwarzschild t, exterior:
    rt_ext <- as.matrix(expand.grid(
        t = c(NA,seq(from=-4,to=4,len=1000)),
        r = r_emitting
    ))[,2:1]
    jj <- TX(rt_ext,exterior=TRUE)
    points(jj,type='l',lwd=0.5,lty=1,col=constant_t_exterior)  # timelike

    ## Antiuniverse, curves of constant Schwarzschild t, exterior:
    jj[,1] <- -jj[,1]
    points(jj,type='l',lwd=0.5,lty=1,col=constant_t_exterior)  # timelike

    ## Now spacelike (sic!) curves on the interior:

    rt_int <- as.matrix(expand.grid(
        r = c(NA,seq(from=0,to=1,len=300)),
        t = seq(from=-4,to=4,len=9)
    ))
    jj <- TX(rt_int,exterior=FALSE)
    points(jj,type='l',lty=1,lwd=0.5,col = constant_t_interior)

    ## Antiuniverse:
    jj[,2] <- -jj[,2]
    points(jj,type='l',lty=1,lwd=0.5,col = constant_t_interior)

    ## Now timelike curves, interior
    rt_int <- as.matrix(expand.grid(
        t = c(NA,seq(from=-4,to=4,len=1000)),
        r = #seq(from=0,to=1,len=10)
            c(0.95, 0.8, 0.6, 0.4,0.1)
    ))[,2:1]

    jj <- TX(rt_int,exterior=FALSE)
    points(jj,type='l',lty=1,lwd=0.5,col=constant_r_interior)

    ## white hole:
    jj[,2] <- -jj[,2]
    points(jj,type='l',lty=1,lwd=0.5,col=constant_r_interior)


    ## light curves:

    l <- 10
    for(i in r_emitting){
        X <-  sqrt(i-1)*exp(i/2)  # KS coordinate 'X'
        segments(x0=X,y0=0,  # start point
                 x1 = 0.5*(X-1/X), y1=-0.5*(-X-1/X), col=colours$ingoing_light)  # ingoing
        segments(x0=X,y0=0,  # start point
                 x1=X+l,y1=X+10,col=colours$outgoing_light)  
    }


    ## light curve originating in the antiuniverse:
    segments(x0=-1,y0=0,x1=0,y1=1,col=colours$ingoing_light)
    segments(x0=-1,y0=0,x1=-2,y1=1,col=colours$outgoing_light)


    ## label some timelike curves on the exterior
    if(FALSE){
        text(0.53,-0.22,labels=paste("r = ",r_emitting[1],sep=""),col=colours$t,srt=-60)
        text(1.33,-0.3,labels=paste("r = ",round(r_emitting[2],2),sep=""),col=colours$t,srt=-80)
        text(2.05,-0.3,labels=paste("r = ",round(r_emitting[3],2),sep=""),col=colours$t,srt=-82)

        ## label some spacelike curves on the exterior
        text(0.7,0.06,labels=paste("t = ",0,sep=""),col=colours$r,srt=0)
        text(0.8,-0.3,labels=paste("t = ",-1,sep=""),col=colours$r,srt=-25)
        text(1.1,-0.75,labels=paste("t = ",-2,sep=""),col=colours$r,srt=-35)
        text(1.7,-1.45,labels=paste("t = ",-3,sep=""),col=colours$r,srt=-40)
        text(1,0.53,labels=paste("t = ",1,sep=""),col=colours$r,srt=25)
        text(1.1,0.9,labels=paste("t = ",2,sep=""),col=colours$r,srt=35)

        ## label some spacelike curves on the interior (constant t [sic])
        text(0.05,0.5,labels=paste("t = ",0,sep=""),col=colours$r,srt=90)
        text(0.22,0.6,labels=paste("t = ",1,sep=""),col=colours$r,srt=64)
        text(0.60,0.9,labels=paste("t = ",2,sep=""),col=colours$r,srt=52)

        ## label some timelike curves on the interior (constant t [sic])
        text(-0.39,0.6,labels=paste("r = ",0.95,sep=""),col=colours$t,srt=-35)
        text(-0.1,0.74,labels=paste("r = ",0.8,sep=""),col=colours$t,srt=-10)
        text(-0.4,0.9,labels=paste("r = ",0.6,sep=""),col=colours$t,srt=-22)
    }

    points(TX(cbind(r_emitting,0),exterior=TRUE),pch=16)
    points(-1,0,pch=16)

    par(lend=1)

    if(FALSE){
        legend(x=-2,y=0.6,lty=1,lwd=5, col=c(colours$horizon, "black"),
               legend=c("horizon","singularity")
               )

        legend(x=-2,y=0,lty=1,lwd=1,
               col=c(colours$ingoing_light,colours$outgoing_light),
               legend=c("ingoing null geodesics","outgoing null geodesics")
               )

        legend(x=-2,y=-0.6,lty=1,lwd=0.5,
               col=c(colours$r,colours$t),
               legend=c("lines of constant Schwarzschild r",
                        "lines of constant Schwarzschild t")
               )

    }

    ## do some light cones:
    jj <-TX(cbind(r_emitting,0),exterior=TRUE)
    cone(jj[1,1],jj[1,2],pi/4,pi/4,size=0.1)
    cone(1.9,1.25,pi/4,pi/4,size=0.1)
    cone(1.7,-0.4,pi/4,pi/4,size=0.1)
    cone(-0.75,0.9,pi/4,pi/4,size=0.1)


    ## The singularity:

    rt_sing <- cbind(r=0,t=seq(from=-4,to=4,len=1000))
    jj <- TX(rt_sing,exterior=FALSE)
    points(jj,type='l',col=colours$singularity,lwd=8)
    polygon(jj,col=colours$singularity_interior)

    ## antisingularity (white hole singularity):
    jj[,2] <- -jj[,2]
    points(jj,type='l',col=colours$singularity,lwd=8)
    polygon(jj,col=colours$singularity_interior)

    ## describe r<0 region:
    text(0,+1.6,expression(r<0),cex=1.6)
    text(0,-1.6,expression(r<0),cex=1.6)


    ## label the universe, antiuniverse, black hole, white hole:

    text(0,-0.6,"white hole")
    text(0,+0.6,"black hole")
    text(-1.6,0.1,"antiuniverse")
    text(1.6,0.1,"universe")
    
    size <- 33
    ## do the horizons last:
    segments(-size,-size,size,size,col=colours$horizon,lwd=5)
    segments(size,-size,-size,size,col=colours$horizon,lwd=5)


    ## plot the AUT logo:
    if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.84,y=0.08, width=0.1)}

    git(-2.4,-2.4)

}
