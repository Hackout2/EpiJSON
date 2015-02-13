#' Convert a dataframe to an ejObject
#' 
#' 
#' @param x The dataframe to convert
#' @param recordAttributes A character vector containing the names of the 
#'  columns in the dataframe that are attributes of the record
#' @param eventDefinitions A list of event definitions
#' @param metadata A list of metadata ejAttribute objects describing the dataset
#' @note We assume one row per record.
#' @export
#' @examples 
#'  library(HistData)
#'  data(Snow.deaths)
#'  #It would be good to have some dates, pumps, some genders 
#'  simulated <- Snow.deaths
#'  simulated$gender <- c("male","female")[(runif(nrow(simulated))>0.5) +1]
#'  simulated$date <- as.POSIXct("1854-04-05") + rnorm(nrow(simulated), 10) * 86400
#'  simulated$pump <- ceiling(runif(nrow(simulated)) * 5)
#'  as.ejObject(simulated, recordAttributes = c("gender"),
#' 		eventDefinitions = list(defineEjEvent(dateStart="date", dateEnd="date", name=NA, location=list(x="x", y="y", proj4string=""), attributes="pump")),
#' 		metadata=list())
as.ejObject.data.frame <- function(x, recordID=NA, recordAttributes, eventDefinitions, metadata=list()){
	#iterate over the dataframe and create an record event for each row
	records <- lapply(1:nrow(x), function(i){
				#grab the attributes
				attributes <- dataFrameToAttributes(x[i,recordAttributes, drop=FALSE])
				
				#now work over the eventDefinitions to get the events
				events <- lapply(eventDefinitions, function(rd){
							createEvent(
									id=notNA(rd$id, x[i,rd$id]),
									name=notNA(rd$name, x[i,rd$name]),
									dateStart=notNA(rd$dateStart, x[i,rd$dateStart]),
									dateEnd=notNA(rd$dateEnd, x[i,rd$dateEnd]),
									location=notNA(rd$location, sp::SpatialPoints(x[i, unlist(rd$location[c("x","y"), drop=FALSE])], proj4string=CRS(rd$location$proj4string))),
									attributes=notNA(rd$attributes, dataFrameToAttributes(x[i,unlist(rd$attributes), drop=FALSE]))
									)
						})
				#fix the event ids
				events <- lapply(1:length(events), function(i){x<-events[[i]]; x$id <- ifelse(is.na(x$id),i,x$id); x})
				
				#grab the record id
				id <- ifelse(is.na(recordID), i, x[i,recordID])
				
				#create and return the record
				createrecord(id, attributes, events)
			})
	createEJObject(metadata, records)
}

#' Creates a event definition
#' 
#' Simplifies the definition of events from columns within a dataframe
#' @param id A character string naming the column that defines the id for a
#'  event. May be NA, and if so will be automatically generated.
#' @param date A character string naming the column that defines the date for
#'  a event. This should be in POSIXct format. May be NA.
#' @param  location A list with entities x, y and proj4string. x and y should be 
#'  character strings naming the columns where the x and y of the location are
#'  defined. crs may be "" or a proj4string.
#' @param attributes A character vector naming the columns for attributes of the
#'  event. The attributes will be named after the columns, with type taken from
#'  column type.
defineEjEvent <- function(id=NA, name=NA, dateStart=NA, dateEnd, location=NA, attributes=NA){
	structure(list(
					id=id,
					name=name,
					dateStart=dateStart,
					dateEnd=dateEnd,
					location=location,
					attributes=attributes
					), class="ejDFeventDef")
}
