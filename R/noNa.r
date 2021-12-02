



# Sum
#' Sum of all the values present in its arguments.
#'
#' ‘Sum’ returns the sum of all the values present in its arguments, omitting NA values. Useful to use inside apply functions to avoid errors due to NA's.
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the sum of the inputs
#'
#' @examples
#' x <- matrix(c((1:11)^2,NA), ncol=4)
#' apply(x,1,Sum)
#'
#' @export
Sum <- function(x)
{
	return(sum(x, na.rm=T))
}



# Mean
#' Mean of all the values present in its arguments.
#'
#' ‘Mean’ returns the mean of all the values present in its arguments, omitting NA values. Useful to use inside apply functions to avoid errors due to NA's.
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the mean of the inputs
#'
#' @examples
#' x <- matrix(c((1:11)^2,NA), ncol=4)
#' apply(x,1,Mean)
#'
#' @export
Mean <- function(x)
{
	return(mean(x, na.rm=T))
}


# Sd
#' Standard deviation of all the values present in its arguments.
#'
#' ‘Sd’ returns the standard deviation of all the values present in its arguments, omitting NA values. Useful to use inside apply functions to avoid errors due to NA's.
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the standard deviation of the inputs
#'
#' @examples
#' x <- matrix(c((1:11)^2,NA), ncol=4)
#' apply(x,1,Sd)
#'
#' @export
Sd <- function(x)
{
	return(sd(x, na.rm=T))
}


# Median
#' Median of all the values present in its arguments.
#'
#' ‘Median’ returns the median of all the values present in its arguments, omitting NA values. Useful to use inside apply functions to avoid errors due to NA's.
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the median of the inputs
#'
#' @examples
#' x <- matrix(c((1:11)^2,NA), ncol=4)
#' apply(x,1,Median)
#'
#' @export
Median <- function(x)
{
	return(median(x, na.rm=T))
}



# Q
#' Quantile-alpha of all the values present in its arguments.
#'
#' ‘Q’ returns the quantile-alpha of all the values present in its arguments, omitting NA values. 
#'
#' @param x A vector containing numeric values
#' @param alpha A numeric value in [0,1] determining the probability. If alpha=0.5, the function returns the median. If alpha=0.25, the function returns quantile 25%.
#'
#' @return A numeric scalar, the quantile-alpha of the inputs
#'
#' @export
Q <- function(x, alpha)
{
	return(quantile(x, probs=alpha, na.rm=T))
}


# Q25
#' Quantile-25 of all the values present in its arguments.
#'
#' ‘Q25’ returns the quantile-25 of all the values present in its arguments, omitting NA values. 
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the quantile-25 of the inputs
#'
#' @export
Q25 <- function(x, alpha)
{
	return(quantile(x, probs=0.25, na.rm=T))
}


# Q75
#' Quantile-75 of all the values present in its arguments.
#'
#' ‘Q75’ returns the quantile-75 of all the values present in its arguments, omitting NA values. 
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the quantile-75 of the inputs
#'
#' @export
Q75 <- function(x, alpha)
{
	return(quantile(x, probs=0.75, na.rm=T))
}




# Max
#' Maximum of all the values present in its arguments.
#'
#' ‘Max’ returns the maximum of all the values present in its arguments, omitting NA values. Useful to use inside apply functions to avoid errors due to NA's.
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the maximum of the inputs
#'
#' @examples
#' x <- matrix(c((1:11)^2,NA), ncol=4)
#' apply(x,1,Max)
#'
#' @export
Max <- function(x, ...)
{
	l = list(...)
	cumu = fac2num(x)
	if(length(l) >0){
		for(i in 1:length(l)){cumu = c(cumu, fac2num(l[[i]]))}
	}
	return(max(cumu, na.rm=T))
}


# Min
#' Minimum of all the values present in its arguments.
#'
#' ‘Min’ returns the minimum of all the values present in its arguments, omitting NA values. Useful to use inside apply functions to avoid errors due to NA's.
#'
#' @param x A vector containing numeric values
#'
#' @return A numeric scalar, the minimum of the inputs
#'
#' @examples
#' x <- matrix(c((1:11)^2,NA), ncol=4)
#' apply(x,1,Min)
#'
#' @export
Min <- function(x, ...)
{
	l = list(...)
	cumu = fac2num(x)
	if(length(l) >0){
		for(i in 1:length(l)){cumu = c(cumu, fac2num(l[[i]]))}
	}
	return(min(cumu, na.rm=T))
}


# lim
#' Borders (max and min) of all the values present in its arguments.
#'
#' ‘lim’ returns a vector containing the minimum and the maximum values of all the values present in its arguments, omitting NA values. Useful to use automatically determine xlim's and ylim's for plots.
#'
#' @param x A vector containing numeric values
#'
#' @return One (or more) vector(s) of numeric values, the minimum and the maximum of the inputs
#'
#' @examples
#' x <- c(1,2,3,4,5,NA,7,8,9,NA,11,12)
#' y <- x^2
#' z <- x^3
#' plot(x,y,xlim=lim(x),ylim=lim(y,z))
#' lines(x,z)
#'
#' @export
lim <- function(...)
{
	return(c(Min(...), Max(...)))
}










