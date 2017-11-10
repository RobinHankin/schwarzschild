## defines forward() and backward(), that is, functions to and from
## Penrose coordinates.  Function penrose_transform() requires
## variable 'choice' to specify the PDF which is used for the
## transformation; this must be one of 'cauchy' 'norm', 'logistic', or
## 'laplace'.  Argument 'forward' is Boolean with TRUE meaning to
## return the forward transformation (that is, takes xt and returns
## uv) or the backward transformation (that is, takes uv and returns
## xt).


penrose_transform <- function(choice,forward){

  potential_choices <- c("cauchy","norm","logistic", "laplace")  # possible values for 'choice'
  stopifnot(choice %in% potential_choices)

  if(choice == "cauchy"){
    fun_forward <- function(x){atan(x)*2/pi}
    fun_backward <- function(x){tan(x*pi/2)}
  } else if(choice == "norm"){
    alpha <- 0.2
    fun_forward <- function(x){2*pnorm(alpha*x)-1}
    fun_backward <- function(x){alpha*qnorm((x+1)/2)}
  } else if(choice == "logistic"){
    alpha <- 1
    fun_forward <- function(x){2*plogis(alpha*x)-1}
    fun_backward <- function(x){alpha*qlogis((x+1)/2)}
  } else if(choice == "laplace"){
    alpha <- 1
    plaplace <- function(x){ifelse(x<0,exp(x)/2,1-exp(-x)/2)}
    qlaplace <- function(x){ifelse(x<1/2,log(2*x),-log(2*(1-x)))}
    fun_forward <- function(x){2*plaplace(alpha*x)-1}
    fun_backward <- function(x){alpha*qlaplace((x+1)/2)}
  } else {
    stop("choice not recognised")
  }
  
  if(forward){
    out <- function(xt){  # takes xt, returns uv
    a1 <- fun_forward(xt[,1]+xt[,2])
    a2 <- fun_forward(xt[,1]-xt[,2])
    cbind(
        u=a1+a2,
        v=a1-a2
    )/2
    }
  } else {
    out <-  function(uv){   # takes uv, returns xt
      t1 <- fun_backward(uv[,1]+uv[,2])
      t2 <- fun_backward(uv[,1]-uv[,2])
      cbind(
          x=t1+t2,
          t=t1-t2
      )/2
    }
  }

  return(out)
}
