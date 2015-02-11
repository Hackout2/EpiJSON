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

processRecord <- function(xx){
	
}

processRecordFrame <- function(x){
	
}

dataFrameToAttributes <- function(x){
	attributeNames <- names(x)
	attributeTypes <- as.vector(sapply(x, typeof))
	
	result <- apply(expand.grid(i=1:nrow(x), j=1:ncol(x)), 1, function(attpos){
				createAttribute(attributeNames[attpos[2]], attributeTypes[attpos[2]], x[attpos[1],attpos[2]])
			})
		
}

createAttribute <- function (name, type, value){
	structure(list(
		name=name,
		type=type,
		value=value
		), class="ejAttribute")
}
