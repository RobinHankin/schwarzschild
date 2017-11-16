## creates all the svg files

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
