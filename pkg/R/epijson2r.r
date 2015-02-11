#' first go at getting JSON
#' 
#' output an \code{obkData} object or part of one to a JSON string or file


#' @param file an epijson filename to convert to R
#' #@param file optional filename for JSON string
#' 
#' @return ?maybe a string of the JSON
#' @export
#' 

#example file file <- 'C://rsprojects//format//pkg//inst//extdata//example.JSON'
epijson2r <- function(file)
{
  
  #first test output the string
  cat(file)
  
  ## convert from json to list
  listjson <- fromJSON(file)
  
  
  
  return(listjson)
}