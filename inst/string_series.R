f0 <- function(x){2*x^(0.5) + x^(3/2)/3 - x^(5/2)/20 }
f0 <- function(x){2*x^(0.5) + x^(3/2)/3  }

fi <- function(x){x + (1+2*log(2)+log(x))/2 +1/(8*x) - 1/(32*x^2)}
fi <- function(x){x + (1+2*log(2)+log(x))/2 +1/(8*x)}
f <- function(x){sqrt(x*(1+x)) + log(1+x)/2 + log(1+sqrt(x/(1+x)))}

x <- seq(from=0,to=2,len=1000)
plotter <- function(...){
plot(x+1,f(x),type='l',col='gray',lwd=10,
     xlab = "Schwarzschild r",
     ylab = "string length")
points(x+1,f0(x),col='red',type='l')
y <- fi(x)
wanted <- (y > 2.5) & (x < 0.5) # avoids overplotting legend
y[wanted] <- NA
points(x+1,y,col='blue',type='l')
points(x+1,x,type='l')
legend("topleft",
       lwd=c(10,1,1,1),
       col=c("gray","red","blue","black"),
       legend=c("true","series (0)","series (inf)","classical"))
}

plotter()
pdf(file="string_approx.pdf")
plotter()
dev.off()