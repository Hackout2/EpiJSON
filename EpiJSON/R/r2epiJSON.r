#' first go at getting JSON from R list of lists
#' 
#' output an \code{obkData} object or part of one to a JSON string or file


#' @param file an R list of lists to an epiJSON file 
#' #@param file optional filename for R list of lists
#' 
#' @return an epiJSON file
#' @export
#' 


## r2epiJSON = convert a list in R to JSON (a series of lists)


r2epiJSON<- function(listJSON)
{
  ##convert from list to JSON
  epiJSON<-RJSONIO::toJSON(listJSON)
  
  return(epiJSON)
}



