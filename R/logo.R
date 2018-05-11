`logo` <- function(...){
    "AUT-logo-block.ps.xml"                  %>%
        system.file(package="schwarzschild") %>%
        readPicture                          %>%
        grid.picture(...)
}
