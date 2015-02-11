#' converting epiJSON into a list of lists
#' 
#' takes an epiJSON string or file and converts to a list of lists


#' @param file an epiJSON filename or string to convert to R
#' #@param file optional filename for JSON string
#' 
#' @return a list of lists of the epijson content
#' @export
#' 

#example file file <- 'C://rsprojects//format//pkg//inst//extdata//example.JSON'
epiJSON2r <- function(file)
{
  
  #first test output the string
  cat(file)
  
  ## convert from json to list
  listJSON <- RJSONIO::fromJSON(file)
 
  
# this might be needed later to get data into a dataframe
# suitable for an obkData object
#   ## get data into a data.frame ##
#   ## get all fields
#   temp <- lapply(listjson, unlist, recursive=TRUE)
#   allfields <- unique(unlist(lapply(temp, names)))
#   
#   ## get data into a data.frame
#   dFindividuals <- matrix(unlist(lapply(temp, f1)), nrow=length(listjson), byrow=TRUE)
#   dFindividuals <- as.data.frame(dFindividuals)
#   names(dFindividuals) <- allfields
  
  
  return(listJSON)
}