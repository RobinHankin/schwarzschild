## Check functionality of the schwarzschild package

test_that("Test suite aab.R",{

  `checker` <- function(x,choice){
    f_forward <- penrose_transform(choice)
    f_backward <- penrose_transform_backward(choice)

    expect_true(all(abs(x-f_forward(f_backward(x)))<1e-10))
    expect_true(all(abs(x-f_backward(f_forward(x)))<1e-10))
  }

  for(choice in c("cauchy","norm","logistic", "laplace")){
    for(i in 1:10){
      checker(matrix(runif(10),5,2)/10,choice)
    } 
  }
    
})
