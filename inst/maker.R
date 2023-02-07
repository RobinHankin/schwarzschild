## Creates all the svg files and all the pdf files, correctly sized;
## type either 'make' or ' R CMD BATCH maker.R'
## at the command line.  Alternatively, in an R session, type
## 'source("maker.R")'

## The AUT logo is included by default.  To switch it off, uncomment
## the line below:

## options("AUTlogo" = FALSE)


library(schwarzschild)

`do` <- function(command, basename){
    ## Function do() is just a convenience wrapper to create both a
    ## .pdf and a .svg file from the same set of commands.

  if(missing(basename)){
    basename <- sub('\\(.*$','',command)  # up to but not including first bracket
}

    pdf(file=paste(basename,"pdf",sep="."), height=9,width=9)
    eval(parse(text=command))
    dev.off()
    
    svg(file=paste(basename,"svg",sep="."), height=9,width=9)
    eval(parse(text=command))
    dev.off()

    jpeg(file=paste(basename,"jpg",sep="."), height=9,width=9,units="in",res=300)
    eval(parse(text=command))
    dev.off()



}

do("schwarzschild()")
do("schwarzschild(draw_infalling_drops = TRUE)", "schwarzschildstring_with_drops")

do("schwarzschild_stringcoords()")
do("schwarzschild_stringcoords(draw_infalling_drops = TRUE)", "schwarzschildstring_with_drops")

do("gullstrand(draw_infalling_drops = FALSE)","gullstrand")
do("gullstrand(draw_infalling_drops = TRUE)", "gullstrand_with_drops")

do("gullstrand_stringcoords(draw_infalling_drops = FALSE)","gullstrand_stringcoords")
do("gullstrand_stringcoords(draw_infalling_drops = TRUE)", "gullstrand_stringcoords_with_drops")

do("eddington()")
do("eddington(draw_infalling_drops = TRUE)")
do("eddington_outgoing()")
do("eddington_outgoing(draw_infalling_drops = TRUE)")

do("eddington_stringcoords()")
do("eddington_stringcoords(draw_infalling_drops=TRUE)")
do("kruskal()")
do("kruskal_with_throw(draw_schwarzschild = FALSE)")
do("kruskal_extended(draw_constant_schwarzschild = TRUE)")
do("kruskal_inverted()")
do("lemaitre(draw_schwarzschild=FALSE)")
do("lemaitre(draw_schwarzschild=TRUE)","lemaitre_with_schwarzschild")
do("penrose_cauchy()")
do("penrose_laplace()")
do("penrose_logistic()")
do("penrose_norm()")
do("penrose_BH_cauchy()")
do("penrose_BH_laplace()")
do("penrose_BH_logistic()")
do("penrose_BH_norm()")
do("penrose_BH_extended()")
do("thrower('','topright')", "thrower")
do("thrower('x')","thrower_x")
do("thrower('y','bottomright')","thrower_y")
do("thrower('xy')","thrower_xy")

do(
    command="
thrower_asp1()
par(xpd=TRUE)
text(5,-1.2,'Schwarzschild r')
text(-1,7.5,'Schwarzschild t',srt=90)
",
basename="thrower_asp1")


pdf(file="allplots.pdf",height=9,width=9)
schwarzschild()
schwarzschild(draw_infalling_drops = TRUE)
schwarzschild_stringcoords()
schwarzschild_stringcoords(draw_infalling_drops = TRUE)
gullstrand(draw_infalling_drops = FALSE)
gullstrand(draw_infalling_drops = TRUE)
gullstrand_stringcoords(draw_infalling_drops = FALSE)
gullstrand_stringcoords(draw_infalling_drops = TRUE)
eddington()
eddington_outgoing()
eddington_stringcoords()
eddington_stringcoords(draw_infalling_drops = TRUE)

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


