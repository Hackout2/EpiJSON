
#' This function takes a single individual from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An individual from the obkData 
#' 
#' @example
#' ##from utils.R run the dataFrameToAttributes function
#' 
#' ##install OutbreakTools package from CRAN repository
#' install.packages(OutbreakTools)
#' require('OutbreakTools')
#' 
#' ##An example dataset is available:
#'      data(ToyOutbreak)
#' 
#'      x <- subset(ToyOutbreak,1)
#'      
#'      processIndividual(x)
#' 
#' @return an ejIndividual
#'

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


#' This function processes Records from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An individual from the obkData 
#' @param recordFrameName The record of interest
#' 
#' @example
#' ##from utils.R run the dataFrameToAttributes function
#' 
#' ##install OutbreakTools package from CRAN repository
#' install.packages(OutbreakTools)
#' require('OutbreakTools')
#' 
#' ##An example dataset is available:
#'    data(ToyOutbreak)
#'
#'    x=subset(ToyOutbreak,2)@records[[1]]
#' 
#'    processRecordFrame(x,"Fever")
#' 
#' @return an ejRecord
#' 

#' Process an individual record frame
processRecordFrame <- function(x, recordFrameName){	
	lapply(1:nrow(x), function(i){
		recordAttributes <- dataFrameToAttributes(x[i,3:ncol(x), drop=FALSE])
		createRecord(id=NA, date=x$date[i], name=recordFrameName, location=NA, attributes=recordAttributes)
	})	
}


#' This function processes objects from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An individual from the obkData 
#' @param metadata The list of the components in the metadata
#' 
#' @example
#' ##from utils.R run the dataFrameToAttributes function
#' 
#' ##install OutbreakTools package from CRAN repository
#' install.packages(OutbreakTools)
#' require('OutbreakTools')
#' 
#' ##An example dataset is available:
#'    data(ToyOutbreak)
#'
#'    x=subset(ToyOutbreak,2)
#' 
#'    as.ejObject.obkData(x,metadata=list())
#' 
#' @return an ejObject
#' 

as.ejObject.obkData <- function(x, metadata=list()){
	individuals <- lapply(get.individuals(x), function(xx){
		processIndividual(subset(x, xx))		
	})

	createEJObject(metadata=metadata, individuals=individuals)
}
