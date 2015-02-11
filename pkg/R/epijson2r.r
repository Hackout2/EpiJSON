#' first go at getting JSON
#' 
#' output an \code{obkData} object or part of one to a JSON string or file


#' @param file an epijson filename to convert to R
#' #@param file optional filename for JSON string
#' 
#' @return a list of lists of the epijson content
#' @export
#' 

#example file file <- 'C://rsprojects//format//pkg//inst//extdata//example.JSON'
epijson2r <- function(file)
{
  
  #first test output the string
  cat(file)
  
  ## convert from json to list
  listjson <- fromJSON(file)
 
  
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
  
  
  return(listjson)
}