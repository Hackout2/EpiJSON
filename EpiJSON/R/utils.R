#' The general functions that are used in multiple sections of the epiJSON package
#' 
#' @param x A dataframe  
#' 
#' @examples
#' dF<- data.frame(id=c("A","B","3D"),name=c("tom","andy","ellie"),
#'                 dob=c("1984-03-14","1985-11-13","1987-06-16"),
#'                 gender=c("male","male","female"),
#'                 rec1contact=c(2,1,5),
#'                 rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1risk=c("high","high","low"),
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"),stringsAsFactors=FALSE)  
#'                 
#' dataFrameToAttributes(dF)
#'   
#' @return result A list of attributes that comprise the dataframe
#' Convert a dataframe to attributes
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

