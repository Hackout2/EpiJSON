print.ejAttribute <- function(x, ...){
	cat("(name: ", x$name, " type:", x$type, " value:", x$value, ")\n")
}
processIndividual <- function(x){
	#get the individual ID
	individualID <- row.names(x@individuals)
	
	#extract individual attributes
	individualAttributeNames <- names(x@individuals)
	individualAttributeTypes <- as.vector(sapply(x@individuals, typeof))
	individualAttributeValues <- x@individuals
	
	#convert to attributes
	attributes <- lapply(1:length(individualAttributeNames), function(i){
		structure(list(
			name=individualAttributeNames[i],
			type=individualAttributeTypes[i],
			value=individualAttributeValues[[i]]
	), class="ejAttribute")
	})
}