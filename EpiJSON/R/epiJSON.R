#' Make as.ejObject generic
as.ejObject <- function(x, ...) UseMethod("as.ejObject")

#'@export 
as.ejObject.default <- function(x){
	stop("I don't know how to convert that object")
}