# devtools::document()



# Return values of Y for a specific country

cY <- function(country)
{
	return(Y[rownames(Y)==country,])
}

# graph of Y for the country n

pY <- function(n)
{
	if(is.numeric(n)){
		plot(time, Y[n,], main=rownames(Y)[n], type='l') 
	}else if(is.character(n)){
		plot(time, cc(Y,n), main=n, type='l' )
	}
}






