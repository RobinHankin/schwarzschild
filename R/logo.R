`logo` <- function(...){
    "AUT-logo-block.ps.xml"                  %>%
        system.file(package="schwarzschild") %>%
        readPicture                          %>%
        grid.picture(...)
}

`git` <- function(x,y,...){
    par(family="mono")
    text(x,y,'https://github.com/RobinHankin/schwarzschild.git',pos=4,cex=0.6,...)
}
