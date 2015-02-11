print.ejAttribute <- function(x, ...){
	cat("(name: ", x$name, " type:", x$type, " value:", x$value, ")\n")
}

print.ejRecord <- function(x, ...){
	cat("Record:\n")
	cat("id: ", x$id, "\n")
	cat("name:", x$name, "\n")
	cat("date: ", x$date, "\n")
	cat("location: ", x$location, "\n")
	for(attribute in x$attributes){print.ejAttribute(attribute)}
}

print.ejIndividual <- function(x, ...){
	cat("Individual:\n")
	cat("id:", x$id,"\n")
	for(attribute in x$attributes){print.ejAttribute(attribute)}
	for(record in x$records){print.ejRecord(record)}
}

print.ejMetadata <- function(x,...){
	cat("MetaData:\n")
	for(attribute in x){print.ejAttribute(attribute)}
}

print.ejObject <- function(x, ...){
	cat("EpiJSON object\n")
	print.ejMetadata(x$metadata)
	for(individual in x$individuals){print.ejIndividual(individual)}
}