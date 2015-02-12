#' A data format for epidemiological data
#'
#' a JSON format for epidemiological data with functions to convert to and from other useful formats  
#' 
#' @import OutbreakTools, jsonlite
#' @docType package
#' @name EpiJSON

#' generic as function
#' @export
as.ejObject <- function(x, ...) UseMethod("as.ejObject")

as.ejObject.default <- function(x){
	stop("I don't know how to convert that object")
}