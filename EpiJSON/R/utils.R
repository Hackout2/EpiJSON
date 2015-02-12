#' Convert a dataframe to attributes
#' 
#'
dataFrameToAttributes <- function(x){
	#get the attrubute name and type from the columns
	attributeNames <- names(x)
	attributeTypes <- as.vector(sapply(x, typeof))
	
	#iterate over the expanded grid and create an attribute for each data position
	result <- apply(expand.grid(i=1:nrow(x), j=1:ncol(x)), 1, function(attpos){
				createAttribute(attributeNames[attpos[2]], attributeTypes[attpos[2]], x[attpos[1],attpos[2]])
			})
	return(result)
}

#' Return a value only if another is not NA
#' 
#' @param x The value to test for NA
#' @param trueValue The value to return if x is not NA
#' @return NA if x is NA or trueValue if x is not NA
notNA <- function(x, trueValue){
	if(!is.na(x[1])){
		return(trueValue)
	} else {
		return(NA)
	}
}