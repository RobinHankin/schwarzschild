`GW` <- function(plus=0.03, cross=0, lattice=TRUE, frames=20, n=11, center=c(0,0)){
    
    jj <- seq(from= -1,to= 1,len=n)

    if(lattice){
        p <- as.matrix(expand.grid(jj,jj))
    } else {
        jj <- jj*(1-1/n)*pi
        p <- cbind(sin(jj),cos(jj))*0.9
    }
    
    par(pty="m") # forces a square plot
    animation::ani.record(reset = TRUE)  # clear history before recording
    
    for(time in seq(from=0,to=2*pi*(1-1/frames),len=frames)){
        
        plus_perturb <-
            plus*cbind(
                     +exp(1i*time)*(p[,1]-center[1]),
                     -exp(1i*time)*(p[,2]-center[2])
                 )
        
        cross_perturb <- 
            cross*cbind(
                      +exp(1i*time)*(p[,2]-center[2]),
                      +exp(1i*time)*(p[,1]-center[1])
                  )
                
    { # plot commands start...
        plot(NULL,xlim=c(-1,1),ylim=c(-1,1),xlab='',ylab='',asp=1,axes=FALSE,type='n')
#        axis(1,pos=-1.1)
#        axis(2,pos=-1.1)
        points(p+Re(plus_perturb + cross_perturb), type='p')
    } #  plot commands end
        
        animation::ani.record()  # record the current frame
    }  # time loop closes
    
    oopts <- animation::ani.options(interval = 0.05)
    while(TRUE){animation::ani.replay()}
    
} # function definition closes
