print.ejAttribute <- function(x, ...){
	cat("(name: ", x$name, " type:", x$type, " value:", x$value, ")\n")
}

print.ejRecord <- function(x, ...){
	cat("Record:")
	cat("id: ", x$id, "\n")
	cat("name:", x$name, "\n")
	cat("date: ", x$date, "\n")
	cat("location: ", x$location, "\n")
	for(attribute in x$attributes){print.ejAttribute(attribute)}
}

processIndividual <- function(x){
	#get the individual ID
	individualID <- row.names(x@individuals)
		
	#convert to attributes
	attributes <- dataFrameToAttributes(x@individuals)
	
	#process the recordFrames
	recordFrames <- x@records
	records<-c()
	for(recordFrame in recordFrames){
		#skip empty frames
		if (nrow(recordFrame)!=0){
			records
		}
	}
}

#' Process an individual record frame
processRecordFrame <- function(x, recordFrameName){	
	lapply(1:nrow(x), function(i){
		recordAttributes <- dataFrameToAttributes(x[i,3:ncol(x), drop=FALSE])
		createRecord(id=NA, date=x$date[i], name=recordFrameName, location=NA, attributes=recordAttributes)
	})	
}



