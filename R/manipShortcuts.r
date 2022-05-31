# devtools::document()





# pause
#' Pause until 'Enter' is pressed
#'
#' Pause R script until <Enter> is pressed in R, to make some breaks in an .R file.
#'
#' @return Print "Press <Enter> to continue..." to indicate how to resume the execution.
#'
#' @export
pause = function(){
    if (interactive()){
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }else{
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
}




#' @export
first <- function(x)
{
	x[!is.na(x)][1]
}
