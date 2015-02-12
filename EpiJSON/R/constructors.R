#' This package outlines the aspects of the data for EpiJSON
#' 
#' This function defines attributes 
#' output \code{ejAttribute}  
#' 
#' @param name This is the name of the attribute, usually a column name
#' @param type This is the type of information, either string, float, integer, boolean or date
#' @param value This is the value that the data takes for the given attribute
#' 
#' @example
#' df<- data.frame(id=c("A","B","3D"),name=c("tom","andy","ellie"),
#' dob=c("1984-03-14","1985-11-13","1987-06-16"),
#' gender=c("male","male","female"),
#' rec1contact=c(2,1,5),
#' rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#' rec1risk=c("high","high","low"),    
#' rec2contact=c(4,1,1),
#' rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#' rec2risk=c("high","low","high"))
#' 
#' \code createAttribute(name="name",type="int",value=c(df$name))
#' 
#' @return an \code{\link{ejAttribute}} object
#' @export
#' 

#' Create an attribute
#' @export
createAttribute <- function (name, type, value){
	structure(list(
					name=name,
					type=type,
					value=value
			), class="ejAttribute")
}
 
#' 
#' This function defines records 
#' output \code{ejRecord}  
#' 
#' @param id This is the identifier for the record
#' @param name This is the name of the record, usually a column name
#' @param date This is the date on which this record (event) happened
#' @param location This is the location at which this record happened
#' @param attributes This is a concatenated list of attributes associated with this record
#' 
#' @example
#' df<- data.frame(id=c("A","B","3D"),name=c("tom","andy","ellie"),
#' dob=c("1984-03-14","1985-11-13","1987-06-16"),
#' gender=c("male","male","female"),
#' rec1contact=c(2,1,5),
#' rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#' rec1risk=c("high","high","low"), 
#' rec1gender=c("male","male","male")   
#' rec2contact=c(4,1,1),
#' rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#' rec2risk=c("high","low","high"))
#' 
#' \code createRecord(id=NA, name="rec1contact",date=c(df$rec1date,value=c(df$name))
#' 
#' @return an \code{\link{ejRecord}} object
#' @export
#' 

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

#' 
#' This function defines individuals 
#' output \code{ejIndividual}  
#' 
#' @param id This is the unique identifier of the individual, usually a column name and the essential information for any data
#' @param attributes This is a list of attributes associated with this individual
#' @param records This is a list of all records associated with this individual (see example)
#' 
#' @example 
#'              
#'               
#' @return an \code{\link{ejRecord}} object
#' @export
#' 

#' Create individual data
#' @export

createIndividual <- function(id, attributes, records){
	structure(list(
					id=id,
					attributes=attributes,
					records=records
					), class="ejIndividual")
}


createEJObject <- function(metadata, individuals){
	structure(list(
					metadata=metadata,
					individuals=individuals
	), class="ejObject")
}


createMetadata <- function(attributes){
	structure(attributes, class="ejMetadata")
}