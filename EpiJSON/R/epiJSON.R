#' A data format for epidemiological data
#'
#' a JSON format for epidemiological data with functions to convert to and from other useful formats  
#' @import OutbreakTools jsonlite sp
#' @docType package
#' @name EpiJSON
NULL

#' generic as function
#' 
#' @param x an object to convert to EpiJSON
#' @param ... other parameters to pass to the converter
#' @export
as.ejObject <- function(x, ...) UseMethod("as.ejObject")

#' By default we don't know how to convert objects
#' 
#' @param x an object
#' @param ... other parameters passed to the call
#' @export
as.ejObject.default <- function(x, ...){
	stop("I don't know how to convert that object")
}