## creates all the svg files; type
##
##   R CMD BATCH svg_maker.R
##
## at the command line, or in an R session, type
##
##  source("svg_maker.R")
##
##  See also pdf_maker.R

library(schwarzschild)

svg(file="schwarzschild.svg", height=9, width=9)
schwarzschild(draw_infalling_drops = FALSE)
dev.off()

svg(file="schwarzschild2.svg", height=9, width=9)
schwarzschild(draw_infalling_drops = TRUE)
dev.off()

svg(file="gullstrand.svg", height=9, width=9)
gullstrand(draw_infalling_drops = FALSE)
dev.off()

svg(file="gullstrand2.svg", height=9, width=9)
gullstrand(draw_infalling_drops = TRUE)
dev.off()

svg(file="eddington.svg", height=9, width=9)
eddington(colours=standard_colours)
dev.off()

svg(file="eddington.svg", height=9, width=9)
eddington_outgoing(colours=standard_colours)
dev.off()

svg(file="kruskal.svg", height=9, width=9)
kruskal(colours=standard_colours)
dev.off()

svg(file="kruskal_plus.svg", height=9, width=9)
kruskal_with_throw(draw_constant_schwarzschild = FALSE)
dev.off()

svg(file="kruskal_plus2.svg", height=9, width=9)
kruskal_extended(draw_constant_schwarzschild = TRUE)
dev.off()

svg(file="kruskal_inverted.svg", height=9, width=9)
kruskal_inverted(colours=standard_colours)
dev.off()

svg(file="lemaitre.svg", height=9, width=9)
lemaitre(draw_schwarzschild=FALSE)
dev.off()

svg(file="lemaitre2.svg", height=9, width=9)
lemaitre(draw_schwarzschild=TRUE)
dev.off()


svg(file="penrose_cauchy.svg", height=9, width=9)
penrose_cauchy()
dev.off()

svg(file="penrose_laplace.svg", height=9, width=9)
penrose_laplace()
dev.off()

svg(file="penrose_logistic.svg", height=9, width=9)
penrose_logistic()
dev.off()

svg(file="penrose_norm.svg", height=9, width=9)
penrose_norm()
dev.off()

svg(file="penrose_BH_cauchy.svg", height=9, width=9)
penrose_BH_cauchy()
dev.off()

svg(file="penrose_BH_laplace.svg", height=9, width=9)
penrose_BH_laplace()
dev.off()

svg(file="penrose_BH_logistic.svg", height=9, width=9)
penrose_BH_logistic()
dev.off()

svg(file="penrose_BH_norm.svg", height=9, width=9)
penrose_BH_norm()
dev.off()

svg(file="penrose_BH_extended.svg", height=9, width=9)
penrose_BH_extended()
dev.off()


svg(file="thrower.svg",width=9,height=9)
thrower('','topright')
dev.off()

svg(file="thrower_x.svg",width=9,height=9)
thrower('x')
dev.off()

svg(file="thrower_y.svg",width=9,height=9)
thrower('y','bottomright')
dev.off()

svg(file="thrower_xy.svg",width=9,height=9)
thrower('xy')
dev.off()

svg(file="thrower_asp1.svg",width=9,height=9)
thrower_asp1(xlab='', ylab='')
par(xpd=TRUE)
text(5,-1.2,'Schwarzschild r')
text(-1,7.5,'Schwarzschild t',srt=90)
dev.off()


