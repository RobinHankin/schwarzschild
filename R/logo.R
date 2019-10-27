`logo` <- function(...){
    image <- getOption("schwarzschild_logo")
    if(is.null(image)){ image <- "orn20.ps.xml" }
  
#    image                                  %>%
#        system.file(package="schwarzschild") %>%
#        readPicture                          %>%
#        grid.picture(...)

    grid.picture(readPicture(system.file(image,package="schwarzschild")),...)
}

`git` <- function(x,y,...){
    par(family="mono")
    text(x,y,'https://github.com/RobinHankin/schwarzschild.git',pos=4,cex=0.6,...)
}
