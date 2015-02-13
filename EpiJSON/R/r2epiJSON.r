#' Produce a JSON string from an EpiJSON object
#' 
#' Produce a JSON representation of an EpiJSON object. This should conform to
#' the EpiJSON standard.
#' @param x An EpiJSON object
#' @return A character string in EpiJSON format.
#' @export
r2epiJSON<- function(x)
{
  ##convert from list to JSON
  epiJSON<-RJSONIO::toJSON(x)
  
  return(epiJSON)
}



