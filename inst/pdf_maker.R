## creates all the pdf files; type
##
##   R CMD BATCH pdf_maker.R
##
## at the command line, or in an R session, type
##
##  source("pdf_maker.R")
##
##  See also svg_maker.R

library(schwarzschild)

pdf(file="schwarzschild.pdf", height=9, width=9)
schwarzschild(draw_infalling_drops = FALSE)
dev.off()

pdf(file="schwarzschild2.pdf", height=9, width=9)
schwarzschild(draw_infalling_drops = TRUE)
dev.off()

pdf(file="gullstrand.pdf", height=9, width=9)
gullstrand(draw_infalling_drops = FALSE)
dev.off()

pdf(file="gullstrand2.pdf", height=9, width=9)
gullstrand(draw_infalling_drops = TRUE)
dev.off()

pdf(file="eddington.pdf", height=9, width=9)
eddington(colours=standard_colours)
dev.off()

pdf(file="eddington.pdf", height=9, width=9)
eddington_outgoing(colours=standard_colours)
dev.off()

pdf(file="kruskal.pdf", height=9, width=9)
kruskal(colours=standard_colours)
dev.off()

pdf(file="kruskal_plus.pdf", height=9, width=9)
kruskal_with_throw(draw_constant_schwarzschild = FALSE)
dev.off()

pdf(file="kruskal_plus2.pdf", height=9, width=9)
kruskal_extended(draw_constant_schwarzschild = TRUE)
dev.off()

pdf(file="kruskal_inverted.pdf", height=9, width=9)
kruskal_inverted(colours=standard_colours)
dev.off()

pdf(file="lemaitre.pdf", height=9, width=9)
lemaitre(draw_schwarzschild=FALSE)
dev.off()

pdf(file="lemaitre2.pdf", height=9, width=9)
lemaitre(draw_schwarzschild=TRUE)
dev.off()


pdf(file="penrose_cauchy.pdf", height=9, width=9)
penrose_cauchy()
dev.off()

pdf(file="penrose_laplace.pdf", height=9, width=9)
penrose_laplace()
dev.off()

pdf(file="penrose_logistic.pdf", height=9, width=9)
penrose_logistic()
dev.off()

pdf(file="penrose_norm.pdf", height=9, width=9)
penrose_norm()
dev.off()

pdf(file="penrose_BH_cauchy.pdf", height=9, width=9)
penrose_BH_cauchy()
dev.off()

pdf(file="penrose_BH_laplace.pdf", height=9, width=9)
penrose_BH_laplace()
dev.off()

pdf(file="penrose_BH_logistic.pdf", height=9, width=9)
penrose_BH_logistic()
dev.off()

pdf(file="penrose_BH_norm.pdf", height=9, width=9)
penrose_BH_norm()
dev.off()

pdf(file="penrose_BH_extended.pdf", height=9, width=9)
penrose_BH_extended()
dev.off()


pdf(file="thrower.pdf",width=9,height=9)
thrower('','topright')
dev.off()

pdf(file="thrower_x.pdf",width=9,height=9)
thrower('x')
dev.off()

pdf(file="thrower_y.pdf",width=9,height=9)
thrower('y','bottomright')
dev.off()

pdf(file="thrower_xy.pdf",width=9,height=9)
thrower('xy')
dev.off()

pdf(file="thrower_asp1.pdf",width=9,height=9)
thrower_asp1(xlab='', ylab='')
par(xpd=TRUE)
text(5,-1.2,'Schwarzschild r')
text(-1,7.5,'Schwarzschild t',srt=90)
dev.off()

