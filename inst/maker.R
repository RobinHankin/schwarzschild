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
do("schwarzschild_with_drops", "schwarzschild(draw_infalling_drops = TRUE)")
do("gullstrand", "gullstrand(draw_infalling_drops = FALSE)")
do("gullstrand_with_drops", "gullstrand(draw_infalling_drops = TRUE)")
do("eddington", "eddington()")
do("eddington_outgoing", "eddington_outgoing()")
do("kruskal", "kruskal()")
do("kruskal_with_throw", "kruskal_with_throw(draw_schwarzschild = FALSE)")
do("kruskal_extended", "kruskal_extended(draw_constant_schwarzschild = TRUE)")
do("kruskal_inverted", "kruskal_inverted()")
do("lemaitre", "lemaitre(draw_schwarzschild=FALSE)")
do("lemaitre_with_schwarzschild", "lemaitre(draw_schwarzschild=TRUE)")
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


pdf(file="allplots.pdf",height=9,width=9)
schwarzschild()
schwarzschild(draw_infalling_drops = TRUE)
gullstrand(draw_infalling_drops = FALSE)
gullstrand(draw_infalling_drops = TRUE)
eddington()
eddington_outgoing()
kruskal()
kruskal_with_throw(draw_schwarzschild = FALSE)
kruskal_extended(draw_constant_schwarzschild = TRUE)
kruskal_inverted()
lemaitre(draw_schwarzschild=FALSE)
lemaitre(draw_schwarzschild=TRUE)
penrose_cauchy()
penrose_laplace()
penrose_logistic()
penrose_norm()
penrose_BH_cauchy()
penrose_BH_laplace()
penrose_BH_logistic()
penrose_BH_norm()
penrose_BH_extended()
thrower('','topright')
thrower('x')
thrower('y','bottomright')
thrower('xy')

thrower_asp1()
par(xpd=TRUE)
text(5,-1.2,'Schwarzschild r')
text(-1,7.5,'Schwarzschild t',srt=90)

dev.off()


