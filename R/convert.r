# devtools::document()




# fac2num
#' Convert a factor to numeric
#'
#' Convert a factor with numeric levels to a non-factor
#'
#' @param x A vector containing a factor with numeric levels
#'
#' @return The input factor made a numeric vector
#' @family convert
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
    return(x)
}






# df2mat
#' Convert a dataframe to matrix
#'
#' Convert a dataframe including numeric values to a matrix; non-numeric columns will be deleted.
#'
#' @param df A dataframe including at least one column of numeric values
#'
#' @return A matrix made of the numeric columns from the dataframe. The column names associated will be conserved. The row names will be those of the column titled "ISO3", "ISO2", "country", 'countries', 'pays', 'region', 'regions', 'names', 'name', 'id' if it exists. 
#' @family convert
#'
#' @examples
#' Y <- df2mat(dfPolicyRate)
#'
#' @export
df2mat <- function(df)
{
	colnam = colnames(df)
	incl = inclStr(colnam, "ISO3", "ISO2", "country", 'countries', 'pays', 'region', 'regions', 'names', 'name', 'id')
	col = first(incl[[2]])
	if(!is.na(col)){nam = df[,col]}else{nam = 1:length(rownames(df))} 
	# check if two names appeared twice!
	colnum = rep(NA,length(colnam))
	for(i in 1:length(colnam)){
		colnum[i] = is.numeric(df[,i])
	}
	
	mat = as.matrix(df[,colnum])
	rownames(mat) = nam
	return(mat)
}


# long
#' Convert a 'wide' to 'long' format
#'
#' Convert A panel, 'wide' format matrix or dataframe to a 'long' format data frame.
#'
#' @param Y A panel, 'wide' format matrix or data frame. The columns have to represent different periods of time and the rows different entries (countries, regions, people, companies, etc.).
#'
#' @return The 'long' format data frame, with three columns: 'time', 'id' and 'value'.
#' @family convert
#'
#' @examples
#' Y <- df2mat(dfPolicyRate)
#' Yl = long(Y)
#' head(Yl)
#'
#' @export
long <- function(Y){
	if(is.matrix(Y)){
		Ydf = data.frame(Y)
		colnames(Ydf) = colnames(Y)
	}else{Ydf = Y}
	
	long = reshape(Ydf, direction='long', 
		idvar = "id", ids = rownames(Ydf),
		times = colnames(Ydf), timevar = "time",
		varying = list(colnames(Ydf)), sep='', v.names = 'values')
	long = long[,c(3,1,2)]
	long = long[sort(rownames(long)),]
	return(long)
}


# mat2df
#' Convert a matrix to data frame
#'
#' @description
#' Convert a matrix (or a set of matrices and vectors) with numeric values to a data frame.
#' If only Y is provided, convert the matrix Y to a dataframe of similar dimensions.
#' If additional matrices and/or vectors are provided, the output will be a 'long-reshaped' dataframe.
#'
#' @param Y A matrix
#' @param ... An additional set of matrices and vectors. These matrices have to be of similar dimensions than Y. The vectors have to have a similar amount of values than Y or than the number of rows in Y. 
#'
#' @return The data frame made of the matrix or the set of inputs. If multiple inputs are provided, the 'long' format data frame will have with two columns + the number of inputs: 'time' and 'id' follow by 'value', 'values.1', 'values.2', etc.
#' @family convert
#'
#' @examples
#' Y <- df2mat(dfPolicyRate)
#' df = mat2df(Y)
#' head(df)
#'
#' Y <- df2mat(dfPolicyRate)
#' df = mat2df(Y, dim(Y)[2]:1, Y, 1:dim(Y)[2])
#' tail(df)
#'
#' @export
mat2df <- function(Y,...)
{
	addElements = list(...)
	Nc = dim(Y)[1]
	Nt = dim(Y)[2]
	if(length(addElements)==0){
		df = data.frame(rownames(Y),Y)
		colnames(df) = c(id, colnames(Y))
	}else{
		df = long(Y)
		for(i in 1:length(addElements)){
			add = addElements[[i]]
			if(!is.numeric(add)){
				stop(paste('Error: the ',i+1,'-th entry is not numeric', sep=''))
			}
			if(is.matrix(add)){
				if(dim(add)[1] == dim(Y)[1] & dim(add)[2] == dim(Y)[2]){
					df = data.frame(df, values = long(add)[,3])
				}else{
					stop(paste('Error: dimensions of the ',i+1,'-th entry does not match dimensions of the first', sep=''))
				}
			}else if(is.vector(add)){
				if(length(add) == Nc*Nt){
					df = data.frame(df, values = add)
					
				}else if(length(add) == Nt){
					df = data.frame(df, values = rep(add,Nc))
					
				}else{
					stop(paste('Error: do not understand what to do with ',i+1,'-th entry', sep=''))
				}
				
			}else{
				stop(paste('Error: the ',i+1,'-th entry is neither a vector nor a matrix', sep=''))
			}
			
		}
	}
	return(df)
}


