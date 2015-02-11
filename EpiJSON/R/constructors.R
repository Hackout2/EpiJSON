#' Create an attribute
createAttribute <- function (name, type, value){
	structure(list(
					name=name,
					type=type,
					value=value
			), class="ejAttribute")
}

#' Create a record
#' 
#' Paragraph of description
#' @param id The id of the record
#' @return A ejRecord object
#' @export
createRecord <- function(id=NA, name, date, location, attributes){
	structure(list( 
					id=id,
					name=name,
					date=strftime(date, "%Y-%m-%dT%H:%M:%S%z"),
					location=location,
					attributes=attributes
			), class="ejRecord")
}

#' Create an individual
createIndividual <- function(id, attributes, records){
	structure(list(
					id=id,
					attributes=attributes,
					records=records
					), class="ejIndividual")
}