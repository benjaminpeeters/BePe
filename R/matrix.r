


# Delete empty rows

DER <- function(Y)
{
	I = NULL
	for(i in 1:dim(Y)[1]){
		if(sum(is.na(Y[i,])) == dim(Y)[2]){
			I = c(I,i)
		}
	}
	Y = Y[-I,]
}



### Assessment of data availability ###

# function to calculate number of available interest rate estimation at every period
valueTime <- function(data, quiet = TRUE)
{

	# nombre de valeurs à chaque période
	totValue = dim(data)[2]
	totTime = dim(data)[1]
	nbrValue = rep(NA, totTime)
	
	# rm: dim(shortRate)[2] doit être égal à tSize
	for(t in 1:totTime)
	{
		nbrValue[t] = sum(is.na(data)[t,])
	}
	nbrValue = totValue - nbrValue
	
	if(!quiet){ plot(Time, nbrValue)}
	return(nbrValue)
}


# Linear Interpolation

linearInterpolation <- function(Y)
{
	for(i in 1:dim(Y)[1]){
		Y[i,] = approx(time,Y[i,],time)$y
	}
	return(Y)
}


### Spatial Matrices ###

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
normalizationMatrix <- function(W, type, threshold=0)
{
	if(type=='eigenvalues'){
		alpha = max(Mod(eigen(W)$values))
#		alpha = sum(W)/ncol(W)
		W = W/alpha
	}else if(type=='row'){
		n = sqrt(length(W))
		for(i in 1:n){ W[i,] = W[i,]/sum(W[i,])}
	
		for(i in 1:n){for(j in 1:n){
			if(W[i,j] < threshold){ W[i,j] = 0}
		}}
		for(i in 1:n){ W[i,] = W[i,]/sum(W[i,]) }
	}
	
	return(W)
}





