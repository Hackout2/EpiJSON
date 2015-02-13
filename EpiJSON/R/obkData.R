
#' This function takes a single record from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An record from the obkData 
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
#'      processrecord(x)
#' 
#' @return an ejRecord
#'

processrecord <- function(x){
	#get the record ID
	recordID <- row.names(x@records)
		
	#convert to attributes
	attributes <- dataFrameToAttributes(x@records)
	
	#process the eventFrames
	eventFrames <- x@events
	events<-c()
	for(eventFrame in names(eventFrames)){
		#skip empty frames
		if (nrow(eventFrames[[eventFrame]])!=0){
			events <- c(events, processeventFrame(eventFrames[[eventFrame]], eventFrame))
		}
	}
	#fix the event ids
	events <- lapply(1:length(events), function(i){x<-events[[i]]; x$id <- i; x})
	createrecord(id=recordID, attributes, events)
}


#' This function processes events from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An record from the obkData 
#' @param eventFrameName The event of interest
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
#'    x=subset(ToyOutbreak,2)@events[[1]]
#' 
#'    processeventFrame(x,"Fever")
#' 
#' @return an ejEvent
#' 

#' Process an record event frame
processeventFrame <- function(x, eventFrameName){	
	lapply(1:nrow(x), function(i){
		eventAttributes <- dataFrameToAttributes(x[i,3:ncol(x), drop=FALSE])
		createevent(id=NA, date=x$date[i], name=eventFrameName, location=NA, attributes=eventAttributes)
	})	
}


#' This function processes objects from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An record from the obkData 
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
	records <- lapply(get.records(x), function(xx){
		processrecord(OutbreakData::subset(x, xx))		
	})

	createEJObject(metadata=metadata, records=records)
}
