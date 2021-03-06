


# helper
#' Convert a factor to numeric
#'
#' Convert a factor with numeric levels to a non-factor
#'
#' @return The input factor made a numeric vector
#'
#' @examples
#' x <- factor(c(3, 4, 9, 4, 9), levels=c(3,4,9))
#' fac2num(x)
#'
#' @export
helper <- function()
{
	
	cat(' ### list of personal helper functions ### \n')
	cat('  - helperApply: help relative to apply functions \n \n')
	
}


# helperApply
#' Convert a factor to numeric
#'
#' Convert a factor with numeric levels to a non-factor
#'
#' @param x A vector containing a factor with numeric levels
#'
#' @return The input factor made a numeric vector
#'
#' @examples
#' x <- factor(c(3, 4, 9, 4, 9), levels=c(3,4,9))
#' fac2num(x)
#'
#' @export
helperApply <- function()
{
	
	cat(' ### function apply ### \n ')
	cat(' - x <- matrix(c((1:11)^2,NA), ncol=4) \n')
	x <- matrix(c((1:11)^2,NA), ncol=4)
	print(x)
	cat(' - apply(x,1,Sum): ',apply(x,1,Sum),' \n')
	cat(' - apply(x,2,Sum): ',apply(x,2,Sum),' \n')
	
}


helperGraph <- function()
{
	
	
}



helperData <- function()
{
	
	cat('dim(): shows the dimensions of the data frame by row and column \n')
    cat('str(): shows the structure of the data frame \n')
    cat('summary(): provides summary statistics on the columns of the data frame \n')
    cat('colnames(): shows the name of each column in the data frame \n')
    cat('head(): shows the first 6 rows of the data frame \n')
    cat('tail(): shows the last 6 rows of the data frame \n')
    cat('View(): shows a spreadsheet-like display of the entire data frame \n')
	
}




