#' converting epiJSON into a list of lists
#' 
#' takes an epiJSON string or file and converts to a list of lists


#' @param file an epiJSON filename or string to convert to R
#' #@param file optional filename for JSON string
#' 
#' @return a list of lists of the epijson content
#' @examples
#' listJSON <- epiJSON2r( system.file("extdata//example.JSON", package="EpiJSON"))
#' str(listJSON)
#' #from within the package would do this
#' #listJSON <- epiJSON2r("extdata//example.JSON")
#'  
#' @export
#' 

epiJSON2r <- function(file)
{
  
  #first test output the string
  cat(file)
  
  ## convert from json to list
  #listJSON <- RJSONIO::fromJSON(file)
  listJSON <- jsonlite::fromJSON(file) 
  
  
# this might be needed later to get data into a dataframe
# suitable for an obkData object
#   ## get data into a data.frame ##
#   ## get all fields
#   temp <- lapply(listjson, unlist, recursive=TRUE)
#   allfields <- unique(unlist(lapply(temp, names)))
#   
#   ## get data into a data.frame
#   dFrecords <- matrix(unlist(lapply(temp, f1)), nrow=length(listjson), byrow=TRUE)
#   dFrecords <- as.data.frame(dFrecords)
#   names(dFrecords) <- allfields
  
  
  return(listJSON)
}
