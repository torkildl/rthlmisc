###
### rthlmisc -- a package of R functions and objects I use now and then
###
###
###
### You can learn more about package authoring with RStudio at:
###
###   http://r-pkgs.had.co.nz/
###
### Some useful keyboard shortcuts for package authoring:
###
###   Build and Reload Package:  'Ctrl + Shift + B'
###   Check Package:             'Ctrl + Shift + E'
###   Test Package:              'Ctrl + Shift + T'


#' A function to identify elements of vector x in vector y.
#' @param x a vector of items to be identified
#' @param y a vector in which to search for items
#' @examples
#' x <- c("Hello", "world!")
#' y <- c("Hello","blurb", "fnord")
#' x %nin% y
#' nordic <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")
#' scandinavian <- c("Denmark", "Norway", "Sweden")
#' nordic %nin% scandinavian

"%nin%" <- function(x, y) {
    return( !(x %in% y) )
}

atuio <- function() return(as.character(Sys.getenv()["USERDNSDOMAIN"])=="UIO.NO")






