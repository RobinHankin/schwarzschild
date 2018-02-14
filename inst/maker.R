## creates all the svg files and all the pdf files; type
##
##   R CMD BATCH svg_maker.R
##
## at the command line, or in an R session, type
##
##  source("maker.R")



## Function do() is just a convenience wrapper to create both a .pdf
## and a .svg file from the same set of commands.  


library(schwarzschild)

`do` <- function(basename, command){
    pdf(file=paste(basename,"pdf",sep="."), height=9,width=9)
    eval(parse(text=command))
    dev.off()
    
    svg(file=paste(basename,"svg",sep="."), height=9,width=9)
    eval(parse(text=command))
    dev.off()
}

do("schwarzschild", "schwarzschild()")
do("schwarzschild2", "schwarzschild(draw_infalling_drops = TRUE)")
do("gullstrand", "gullstrand(draw_infalling_drops = FALSE)")
do("gullstrand2", "gullstrand(draw_infalling_drops = TRUE)")
do("eddington", "eddington()")
do("eddington_outgoing", "eddington_outgoing()")
do("kruskal", "kruskal()")
do("kruskal_plus", "kruskal_with_throw(draw_constant_schwarzschild = FALSE)")
do("kruskal_plus2", "kruskal_extended(draw_constant_schwarzschild = TRUE)")
do("kruskal_inverted", "kruskal_inverted()")
do("lemaitre", "lemaitre(draw_schwarzschild=FALSE)")
do("lemaitre2", "lemaitre(draw_schwarzschild=TRUE)")
do("penrose_cauchy", "penrose_cauchy()")
do("penrose_laplace", "penrose_laplace()")
do("penrose_logistic", "penrose_logistic()")
do("penrose_norm", "penrose_norm()")
do("penrose_BH_cauchy", "penrose_BH_cauchy()")
do("penrose_BH_laplace", "penrose_BH_laplace()")
do("penrose_BH_logistic", "penrose_BH_logistic()")
do("penrose_BH_norm", "penrose_BH_norm()")
do("penrose_BH_extended", "penrose_BH_extended()")
do("thrower", "thrower('','topright')")
do("thrower_x", "thrower('x')")
do("thrower_y", "thrower('y','bottomright')")
do("thrower_xy", "thrower('xy')")

do("thrower_asp1", command="
thrower_asp1()
par(xpd=TRUE)
text(5,-1.2,'Schwarzschild r')
text(-1,7.5,'Schwarzschild t',srt=90)
")



