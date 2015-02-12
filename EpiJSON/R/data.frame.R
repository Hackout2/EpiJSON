#' Convert a dataframe to an ejObject
#' 
#' 
#' @param x The dataframe to convert
#' @param individualAttributes A character vector containing the names of the 
#'  columns in the dataframe that are attributes of the individual
#' @param recordDefinitions A list of record definitions
#' @param metadata A list of metadata ejAttribute objects describing the dataset
#' @note We assume one row per individual.
#' @examples 
#'  library(HistData)
#'  data(Snow.deaths)
#'  #It would be good to have some dates, pumps, some genders 
#'  simulated <- Snow.deaths
#'  simulated$gender <- c("male","female")[(runif(nrow(simulated))>0.5) +1]
#'  simulated$date <- as.POSIXct("1854-04-05") + rnorm(nrow(simulated), 10) * 86400
#'  simulated$pump <- ceiling(runif(nrow(simulated)) * 5)
#'  as.ejObject(simulated, individualAttributes = c("gender"),
#' 		recordDefinitions = list(defineEJRecord(date="date", name=NA, location=list(x="x", y="y", proj4string=""), attributes="pump")),
#' 		metadata=list())
as.ejObject.data.frame <- function(x, individualID=NA, individualAttributes, recordDefinitions, metadata=list()){
	#iterate over the dataframe and create an individual record for each row
	individuals <- lapply(1:nrow(x), function(i){
				#grab the attributes
				attributes <- dataFrameToAttributes(x[i,individualAttributes, drop=FALSE])
				
				#now work over the recordDefinitions to get the records
				records <- lapply(recordDefinitions, function(rd){
							createRecord(
									id=notNA(rd$id, x[i,rd$id]),
									name=notNA(rd$name, x[i,rd$name]),
									date=notNA(rd$date, x[i,rd$date]),
									location=notNA(rd$location, SpatialPoints(x[i, unlist(rd$location[c("x","y"), drop=FALSE])], proj4string=CRS(rd$location$proj4string))),
									attributes=notNA(rd$attributes, dataFrameToAttributes(x[i,unlist(rd$attributes), drop=FALSE]))
									)
						})
				#fix the record ids
				records <- lapply(1:length(records), function(i){x<-records[[i]]; x$id <- ifelse(is.na(x$id),i,x$id); x})
				
				#grab the individual id
				id <- ifelse(is.na(individualID), i, x[i,individualID])
				
				#create and return the individual
				createIndividual(id, attributes, records)
			})
	createEJObject(metadata, individuals)
}

#' Creates a record definition
#' 
#' Simplifies the definition of records from columns within a dataframe
#' @param id A character string naming the column that defines the id for a
#'  record. May be NA, and if so will be automatically generated.
#' @param date A character string naming the column that defines the date for
#'  a record. This should be in POSIXct format. May be NA.
#' @param  location A list with entities x, y and proj4string. x and y should be 
#'  character strings naming the columns where the x and y of the location are
#'  defined. crs may be "" or a proj4string.
#' @param attributes A character vector naming the columns for attributes of the
#'  record. The attributes will be named after the columns, with type taken from
#'  column type.
defineEJRecord <- function(id=NA, name=NA, date=NA, location=NA, attributes=NA){
	structure(list(
					id=id,
					name=name,
					date=date,
					location=location,
					attributes=attributes
					), class="ejDFRecordDef")
}
