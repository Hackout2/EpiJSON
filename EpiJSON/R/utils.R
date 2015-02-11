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