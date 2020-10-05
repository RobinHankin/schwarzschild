## Check functionality of the schwarzschild package

test_that("Test suite aaa.R",{

  schwarzschild())
  schwarzschild(draw_infalling_drops=TRUE)
  eddington()

  outgoing_null_arrow_eddington_ingoing_coords(r=2,offset= -1)
  outgoing_null_arrow_eddington_ingoing_coords(r=2,offset=  0)
  outgoing_null_arrow_eddington_ingoing_coords(r=2,offset= +1)

  penrose_cauchy()
  penrose_laplace()
  penrose_logistic()
  penrose_norm()
  penrose_BH_cauchy()
  penrose_BH_laplace()
  penrose_BH_logistic()
  penrose_BH_norm()
  penrose_BH_extended()
  
  kruskal()
  kruskal_extended()
  kruskal_with_throw()
  kruskal_with_throw(draw_schwarzschild=TRUE)
  eddington()
  eddington_outgoing()
  
  gullstrand()
  thrower('','topright')
  thrower('x')
  thrower('xy')
  thrower_asp1()

})

