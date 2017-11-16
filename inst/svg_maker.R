## creates all the svg files

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
