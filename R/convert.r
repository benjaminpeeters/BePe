# devtools::document()




# fac2num
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
fac2num <- function(x)
{
    nam <- names(x)
    x <- as.numeric(as.character(x))
    names(x) <- nam
    x
}



# fac2num
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
mat2df <- function(Y,GF)
{
	Nc = dim(Y)[1]
	Nt = dim(Y)[2]
	countries = rep(NA, (Nc*Nt))
	for(i in 1:(Nc*Nt)){
		if((i%%Nt)==0){ u = i%/%Nt }else{ u = i%/%Nt + 1}
		countries[i] = rownames(Y)[u]
	}
	df = data.frame(countries, rep(time,Nc), as.vector(t(Y)), rep(GF,Nc))
	colnames(df) = c("countries","time", "Y", "GF")
	return(df)
}


# fac2num
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
df2mat <- function(y)
{
	Y = matrix(y, ncol=length(time), byrow=TRUE)
}






