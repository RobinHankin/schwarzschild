kruskal_inverted <- function(colours=standard_colours, ...){

    n <- 4 # size of plot
    plot(NULL,xlim=c(0,n),ylim=c(0,n),asp=1,type="n",axes=FALSE,
         xlab='Schwarzschild r',ylab='Schwarzschild t',main="Inverted Kruskal-Szekeres coordinates")
    
    axis(1,pos=0,at=0:n)
    axis(2,pos=0,at=0:n)
    
    for(i in 0:4){ segments(x0=0,x1=n,y0=i,col=colours$t)}
    for(i in 0:4){ segments(x0=i,y0=0,y1=n,col=colours$r)}

    clip(0,n,0,n)
    
    ## Tval and Xval are Kruskal X,T.
    
    Tval <- seq(from=-33,to=33,len=10000)
    Xval <- c(0,0.5,1,2,4,8,16,32)
    
    
    ## first, lines of constant X
    for(i in Xval){
        points(RT(cbind(i,Tval)),type='l',col=colours$kruskal_X)
    }
    
    Tval <- c(0,0.5,1,2,4,8,16)
    Xval <- seq(from=-33,to=33,len=10000)
      
    ## second, lines of constant T
    for(i in Tval){
        points(RT(cbind(Xval,i)),type='l',col=colours$kruskal_T)
    }
    
    legend(x=2.1, y=3.9, lty=1, bg="white",
           legend=c(
               "lines of constant Schwarzschild r",
               "lines of constant Schwarzschild t",
               "lines of constant X",
               "lines of constant T"
           ),
           col=c(
               colours$r,
               colours$t,
               colours$kruskal_X,
               colours$kruskal_T
           )
           )

    ## horizon and singularity last:
    segments(x0=0,y0=0,y1=5,lwd=5,lend=1,col=colours$singularity)
    segments(x0=1,y0=0,y1=5,lwd=5,lend=1,col=colours$horizon)

  ## plot the AUT logo:
  if(!isFALSE(getOption("schwarzschild_logo"))){logo(x=0.84,y=0.08, width=0.1)}


  par(family="mono")
  git(-0.8,-0.7)

}
