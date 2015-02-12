processIndividual <- function(x){
	#get the individual ID
	individualID <- row.names(x@individuals)
		
	#convert to attributes
	attributes <- dataFrameToAttributes(x@individuals)
	
	#process the recordFrames
	recordFrames <- x@records
	records<-c()
	for(recordFrame in names(recordFrames)){
		#skip empty frames
		if (nrow(recordFrames[[recordFrame]])!=0){
			records <- c(records, processRecordFrame(recordFrames[[recordFrame]], recordFrame))
		}
	}
	#fix the record ids
	records <- lapply(1:length(records), function(i){x<-records[[i]]; x$id <- i; x})
	createIndividual(id=individualID, attributes, records)
}

#' Process an individual record frame
processRecordFrame <- function(x, recordFrameName){	
	lapply(1:nrow(x), function(i){
		recordAttributes <- dataFrameToAttributes(x[i,3:ncol(x), drop=FALSE])
		createRecord(id=NA, date=x$date[i], name=recordFrameName, location=NA, attributes=recordAttributes)
	})	
}

as.ejObject.obkData <- function(x, metadata=list()){
	individuals <- lapply(get.individuals(x), function(xx){
		processIndividual(OutbreakData::subset(x, xx))		
	})

	createEJObject(metadata=metadata, individuals=individuals)
}
