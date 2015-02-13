#' Create a SpatialPointsDataframe from an ejObject
#' 
#' @param x An ejObject
as.SpatialPointsDataframe.ejObject(x){
	#gather
}

#convert an ej attribute to a dataframe
as.data.frame.ejAttribute <- function(x){
	result <- data.frame(x$value)
	names(result) <- x$name
	return(result)
}

#'convert an ejEvent to a SpatialPointsDataframe
as.SpatialPointsDataframe.ejEvent <- function(x){
	#grab the columns
	eventCols <- do.call(cbind, c(list(eventId=x$id, name=x$name, date=x$date),lapply(x$attributes, as.data.frame)))
	SpatialPointsDataFrame(testevent$location, eventCols)
}

#'convert an ejRecord to a SpatialPointsDataframe
as.SpatialPointsDataframe.ejRecord <- function(x){
	#convert all the events to SpatialPointsDataframes
	eventList <- lapply(x$events, function(event){
		#only work on those events with a location
		result <- NULL
		if(class(event$location) == "SpatialPoints"){
			result <- as.SpatialPointsDataframe.ejEvent(event)
		}
		result
	})
	
	eventLocations <- do.call(rbind, lapply(eventList, function(x){x@coords}))
	eventData <- do.call(rbind.fill, lapply(eventList, function(x){x@data}))

	#convert the indicidual attributes to columns
	recordAttributes <- do.call(cbind, c(list(recordId=x$id),lapply(x$attributes, as.data.frame)))
	
	
	#here we do something a bit different. Normally we one record per row
	#but for a spatial object we are going to repeat records for each
	#spatial event
	
	#duplicate to match the event data
	recordAttributes <- recordAttributes[rep(1,nrow(eventData)),]
	
	resultDF <- cbind(recordAttributes, eventData)
	#construct the Spatial points dataframe
	SpatialPointsDataframe(eventLocations, resultDF)
}