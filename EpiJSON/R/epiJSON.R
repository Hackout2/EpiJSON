as.ejObject <- function(x, ...) UseMethod("as.ejObject")

as.ejObject.default <- function(x){
	stop("I don't know how to convert that object")
}