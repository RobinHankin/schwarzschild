penrose_BH_extended <- function(light=FALSE, colours = standard_colours, ...){
    
    ## This file creates penrose_BH_extended.pdf
    ## plots a Penrose diagram of the whole universe, including a black hole

    penrose <- penrose_transform("cauchy")


    constant_r_exterior <- colours$r
    constant_t_exterior <- colours$t

    constant_r_interior <- colours$t
    constant_t_interior <- colours$r

    ## set up axes
    plot(c(-1,1),c(-0.5,0.5),asp=1,type='n',axes=FALSE,xlab='',ylab='',main='Maximally extended Penrose diagram of a black hole')


    ## First curves of constant Schwarzschild t, exterior
    rt_ext <- as.matrix(expand.grid(
        r = c(NA,seq(from=1,to=40,len=1000)),   # the NA is so we can just use plot(...,type='l')
        t = seq(from=-4,to=4,len=9)
    ))

    ## plot curves of constant Schwarzschild t [ie spacelike curves] on the exterior:
    jj <- penrose(TX(rt_ext,exterior=TRUE))   # penrose() defined in ../penrose_transform_chooser.R
    points(jj,type='l',lty=1,lwd=0.5,col=colours$t)  # spacelike
    jj[,1] <- -jj[,1]
    points(jj,type='l',lty=1,lwd=0.5,col=colours$t)  # spacelike


    ## Now curves of constant t (which are timelike (sic!)) curves on the
    ## interior:

    rt_int <- as.matrix(expand.grid(
        r = c(NA,seq(from=0,to=1,len=3000)),
        t = seq(from=-4,to=4,len=9)
    ))

    jj <- penrose(TX(rt_int,exterior=FALSE))


    ## lines of constant t (timelike curves [sic]) inside the white hole:
    points(jj,type='l',lty=1,lwd=0.5,col=colours$t)
    jj[,2] <- -jj[,2]

    ## lines of constant t (timelike curves [sic]) inside the white hole:
    points(jj,type='l',lty=1,lwd=0.5,col=colours$t)

    r_values <- c(1.05,1.2,1+lambert_W0(exp(-1))+NA,1.5,2,3)

    ## NB: the third value of r_values gives a perfectly vertical line;
    ## we add NA to suppress the plotting of it (because it makes the
    ## graphic look cluttered).



    ## Now plot lines in the antiuniverse:
    rt_exterior <- as.matrix(expand.grid(
        t = c(NA,seq(from=-10,to=10,len=1000)),
        r = r_values
    ))[,2:1]

    jj <- penrose(TX(rt_exterior,exterior=TRUE))

    # lines of constant r (timelike curves) in the universe:
    points(jj,type='l',lty=1,lwd=0.5,col=constant_t_interior)
    jj[,1] <- -jj[,1]

    ##  lines of constant r (timelike curves) in the antiuniverse:
    points(jj,type='l',lty=1,lwd=0.5,col=constant_t_interior)

  
    ## plot curves of constant Schwarzschild r inside the EH.

    r_values_inside <- c(0.95, 0.8, 0.6, 0.4,0.1)
    rt_int <- as.matrix(expand.grid(
        t = c(NA,seq(from=-10,to=10,len=1000)),
        r = r_values_inside 
    ))[,2:1]

    ## lines of constant r (spacelike curves, interior of the black hole):
    jj <- penrose(TX(rt_int,exterior=FALSE))
    points(jj,type='l',lty=1,lwd=0.5,col=constant_t_interior)

    ## lines of constant r (spacelike curves, interior of the white hole):
    jj[,2] <- -jj[,2]
    points(jj,type='l',lty=1,lwd=0.5,col=constant_t_interior)


    ## plot the singularity:
    segments(x0=-0.5,y0=0.5,x1=0.5,lwd=5,col=colours$singularity)
    segments(x0=-0.5,y0=-0.5,x1=0.5,lwd=5,col=colours$singularity)


    ## draw the boundary of the universe
    segments(x0=0.5,y0=0.5,x1=1,y1=0,lwd=1,col=colours$singularity)
    segments(x0=1,y0=0,x1=0.5,y1=-0.5,lwd=1,col=colours$singularity)
    segments(x0=-0.5,y0=-0.5,x1=-1,y1=0,lwd=1,col=colours$singularity)
    segments(x0=-1,y0=0,x1=-0.5,y1=0.5,lwd=1,col=colours$singularity)

    ## do the horizons last:
    segments(x0=-0.5,y0=0.5,x1=0.5,y1=-0.5, col=colours$horizon,lwd=5)
    segments(x0=-0.5,y0=-0.5,x1=0.5,y1=0.5, col=colours$horizon,lwd=5)
}
