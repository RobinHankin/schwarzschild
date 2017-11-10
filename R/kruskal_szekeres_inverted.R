kruskal_inverted <- function(colours=standard_colours, ...){

plot(NULL,xlim=c(0,4),ylim=c(0,4),asp=1,
     xlab='Schwarzschild r',ylab='Schwarzschild t')


 ## Tval and Xval are Kruskal X,T.

Tval <- seq(from=-33,to=33,len=10000)
Xval <- c(0,0.5,1,2,4,8,16,32,33)


## first, lines of constant X
for(i in Xval){
  points(RT(cbind(i,Tval)),type='l',col=colours$r)
}

abline(v=0)
abline(v=1)


Tval <- c(0,0.5,1,2,4,8,16,17)
Xval <- seq(from=-33,to=33,len=10000)


## second, lines of constant X
for(i in Tval){
  points(RT(cbind(Xval,i)),type='l',col=colours$t)
}

}
