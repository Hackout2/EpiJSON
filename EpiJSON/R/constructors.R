#' Create an attribute
createAttribute <- function (name, type, value){
	structure(list(
					name=name,
					type=type,
					value=value
			), class="ejAttribute")
}

#' Create a record
createRecord <- function(id=NA, name, date, location, attributes){
	structure(list( 
					id=id,
					name=name,
					date=strftime(date, "%Y-%m-%dT%H:%M:%S%z"),
					location=location,
					attributes=attributes
			), class="ejRecord")
}
