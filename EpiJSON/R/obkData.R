
#' This function takes a single record from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An record from the obkData 
#' @examples
#' \dontrun{
#' #because this function is not exported this example won't work outside the package
#' require('OutbreakTools')
#' data(ToyOutbreak)
#' x <- subset(ToyOutbreak,1)  
#' processRecord(x)
#' }
#' 
#' @return an ejRecord
processRecord <- function(x){
	#get the record ID
	recordID <- row.names(x@individuals)
		
	#convert to attributes
	attributes <- dataFrameToAttributes(x@individuals)
	
	#process the record frames to events
	recordFrames <- x@records
	events<-c()
	for(recordFrame in names(recordFrames)){
		#skip empty frames
		if (nrow(recordFrames[[recordFrame]])!=0){
			events <- c(events, processRecordFrame(recordFrames[[recordFrame]], recordFrame))
		}
	}
	#fix the event ids
	events <- lapply(seq_along(events), function(i){x<-events[[i]]; x$id <- i; x})
	createRecord(id=recordID, attributes, events)
}


#' This function processes events from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An record from the obkData 
#' @param recordFrameName The event of interest
#' 
#' @examples
#' ##from utils.R run the dataFrameToAttributes function
#' 
#' ##install OutbreakTools package from CRAN repository
#' #install.packages('OutbreakTools')
#' require('OutbreakTools')
#' 
#' ##An example dataset is available:
#'    data(ToyOutbreak)
#'
#'    x=subset(ToyOutbreak,2)@@events[[1]]
#' 
#'    processeventFrame(x,"Fever")
#' 
#' @return an ejEvent
#' Process an record event frame
processRecordFrame <- function(x, recordFrameName){	
	lapply(1:nrow(x), function(i){
		eventAttributes <- dataFrameToAttributes(x[i,3:ncol(x), drop=FALSE])
		createEvent(id=NA, dateStart=x$date[i], dateEnd=x$date[i], name=recordFrameName, location=NA, attributes=eventAttributes)
	})	
}


#' This function processes objects from the obkClass data and 
#' converts to the epiJSON format
#' 
#' @param x An record from the obkData 
#' @param metadata The list of the components in the metadata
#' @note There is a slight mismatch in symantics here obkData individuals are
#'  equivelent to EpiJSON records and obkData records are EpiJSON events. This
#'  is beause in EpiJSON the unit of record is not necessarily an individual
#'  (it could, for example, be a region or hospital, etc). 
#' @examples
#' require('OutbreakTools')
#' data(ToyOutbreak)
#' x=subset(ToyOutbreak,2)
#' as.ejObject(x, metadata=list())
#' 
#' @return an ejObject
#' @method as.ejObject obkData
#' @export 
as.ejObject.obkData <- function(x, metadata=list()){
	records <- lapply(OutbreakTools::get.individuals(x), function(xx){
		processRecord(OutbreakTools::subset(x, xx))		
	})

	createEJObject(metadata=metadata, records=records)
}

# #' Create an obkData object from an ejObject
# #' 
# #' @export 
# as.obkData.ejObject <-function(){}