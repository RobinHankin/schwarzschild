kruskal_inverted <- function(colours=standard_colours, ...){

plot(NULL,xlim=c(0,4),ylim=c(0,4),asp=1,
     xlab='Schwarzschild r',ylab='Schwarzschild t',main="Inverted Kruskal-Szekeres coordinates")



  abline(h=0:4,col=colours$t)
  abline(v=0:4,col=colours$r)
 ## Tval and Xval are Kruskal X,T.

Tval <- seq(from=-33,to=33,len=10000)
Xval <- c(0,0.5,1,2,4,8,16,32,33)


## first, lines of constant X
for(i in Xval){
  points(RT(cbind(i,Tval)),type='l',col=colours$kruskal_X)
}

abline(v=0)
abline(v=1)


Tval <- c(0,0.5,1,2,4,8,16,17)
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

    abline(v=0,lwd=5)
  abline(v=1,lwd=5,col=colours$horizon)

}
