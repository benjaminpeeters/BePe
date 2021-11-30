# devtools::document()



# Return values of Y for a specific country

cc <- function(Y, country)
{
	return(Y[rownames(Y)==country,])
}

# graph of Y for the country n

plotY <- function(n)
{
	if(is.numeric(n)){
		plot(time, Y[n,], main=rownames(Y)[n], type='l') 
	}else if(is.character(n)){
		plot(time, cc(Y,n), main=n, type='l' )
	}
}






