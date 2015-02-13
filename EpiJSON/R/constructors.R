#' Create an attribute
#' This package outlines the aspects of the data for EpiJSON
#' 
#' This function defines attributes 
#' output \code{ejAttribute}  
#' 
#' @param name This is the name of the attribute, usually a column name
#' @param type This is the type of information, either string, float, integer, boolean or date
#' @param value This is the value that the data takes for the given attribute
#' 
#' @examples
#' dF <- data.frame(id=c("A","B","3D"),
#'                 name=c("tom","andy","ellie"),
#'                 dob=c("1984-03-14","1985-11-13","1987-06-16"),
#'                 gender=c("male","male","female"),
#'                 rec1contact=c(2,1,5),
#'                 rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"))
#' 
#' createAttribute(name="name",type="int",value=list(dF$name))
#' 
#' @return an \code{\link{ejAttribute}} object
#' @export
createAttribute <- function (name, type, value){
	structure(list(
					name=name,
					type=type,
					value=value
			), class="ejAttribute")
}

#' Create a event
#' 
#' This function defines events 
#' output \code{ejEvent}  
#' 
#' @param id This is the identifier for the event
#' @param name This is the name of the event, usually a column name
#' @param date This is the date on which this event (event) happened
#' @param location This is the location at which this event happened
#' @param attributes This is a concatenated list of attributes associated with this event
#' 
#' @examples
#' dF<- data.frame(id=c("A","B","3D"),
#'                 name=c("tom","andy","ellie"),
#'                 dob=c("1984-03-14","1985-11-13","1987-06-16"),
#'                 gender=c("male","male","female"),
#'                 rec1contact=c(2,1,5),
#'                 rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"))
#' 
#' event1<-createevent(id=NA, 
#'              name="rec1contact",
#'              date=as.Date(dF$rec1date),
#'              location="",
#'              attributes=list(createAttribute(name="rec1risk",type="str",value=list(dF$rec1risk)),
#'                              createAttribute(name="rec1temp",type="int",value=list(dF$rec1temp))))
#' 
#' @return an \code{\link{ejEvent}} object
#' @export
createevent <- function(id=NA, name, dateStart, dateEnd, location, attributes){
	structure(list( 
					id=id,
					name=name,
					dateStart=strftime(date, "%Y-%m-%dT%H:%M:%S%z"),
					dateEnd=strftime(date, "%Y-%m-%dT%H:%M:%S%z"),
					location=location,
					attributes=attributes
			), class="ejEvent")
}

#' 
#' This function defines records 
#' output \code{ejRecord}  
#' 
#' @param id This is the unique identifier of the record, usually a column name and the essential information for any data
#' @param attributes This is a list of attributes associated with this record
#' @param events This is a list of all events associated with this record (see example)
#' 
#' @examples
#' dF<- data.frame(id=c("A","B","3D"),
#'                 name=c("tom","andy","ellie"),
#'                 dob=c("1984-03-14","1985-11-13","1987-06-16"),
#'                 gender=c("male","male","female"),
#'                 rec1contact=c(2,1,5),
#'                 rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"))
#' 
#' indiv1<-createrecord(id=dF$id, 
#'              attributes=list(createAttribute(name="name",type="str",value=list(dF$name)),
#'                              createAttribute(name="dob",type="date",value=list(as.Date(dF$dob))),
#'                              createAttribute(name="gender",type="str",value=list(dF$gender))),
#'              events=list(createevent(id=NA, 
#'                                        name="rec1contact",
#'                                        date=as.Date(dF$rec1date),
#'                                        location="",
#'                                              attributes=list(createAttribute(name="rec1risk",type="str",value=list(dF$rec1risk)),
#'                                                              createAttribute(name="rec1temp",type="int",value=list(dF$rec1temp)))
#'                                                                  ),
#'                          createevent(id=NA, 
#'                                       name="rec2contact",
#'                                       date=as.Date(dF$rec2date),
#'                                       location="",
#'                                              attributes=list(createAttribute(name="rec2risk",type="str",value=list(dF$rec2risk)))
#'                                        )
#'                              )  
#'                      )     
#'               
#' @return an \code{\link{ejEvent}} object
#' @export
#' 
createrecord <- function(id, attributes, events){
	structure(list(
					id=id,
					attributes=attributes,
					events=events
					), class="ejRecord")
}


#' 
#' This function defines epiJSON objects 
#' output \code{ejObject}  
#' 
#' @param metadata This is the metadata information for the entire dataset
#' @param records This is the unique records in the dataset
#' 
#' @examples
#' dF<- data.frame(id=c("A","B","3D"),
#'                 name=c("tom","andy","ellie"),
#'                 dob=c("1984-03-14","1985-11-13","1987-06-16"),
#'                 gender=c("male","male","female"),
#'                 rec1contact=c(2,1,5),
#'                 rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"))
#' 
#' obj1<-createEJObject (metadata=list(list(name="ID",type="str",value=dF$id),
#'                                     list(name="name",type="str",value=dF$name),
#'                                     list(name="rec1contact",type="str",value=dF$rec1contact),
#'                                     list(name="rec1date",type="date",value=dF$rec1date)),
#'                                                  records=list(createrecord(id=dF$id, 
#'                                                            attributes=list(createAttribute(name="name",type="str",value=dF$name),
#'                                                                            createAttribute(name="dob",type="date",value=as.Date(dF$dob)),
#'                                                                            createAttribute(name="gender",type="str",value=dF$gender)),
#'                                                                    events=list(createevent(id=NA, 
#'                                                                                              name="rec1contact",
#'                                                                                              date=as.Date(dF$rec1date),
#'                                                                                              location="",
#'                                                                                              attributes=list(createAttribute(name="rec1risk",type="str",value=dF$rec1risk),
#'                                                                                                              createAttribute(name="rec1temp",type="int",value=dF$rec1temp))
#'                                                                                                  ),
#'                                                                                createevent(id=NA, 
#'                                                                                             name="rec2contact",
#'                                                                                             date=as.Date(dF$rec2date),
#'                                                                                             location="",
#'                                                                                             attributes=list(createAttribute(name="rec2risk",type="str",value=dF$rec2risk))
#'                                                                                            )
#'                                                                                )  
#'                                                                            )
#'                                                                )
#'                    ) 
#'                   
#'               
#' @return an \code{\link{ejObject}} object
#' @export
createEJObject <- function(metadata, records){
	structure(list(
					metadata=metadata,
					records=records
	), class="ejObject")
}

#' 
#' This function defines epiJSON Metadata 
#' output \code{ejMetadata}  
#' 
#' @param attributes These are the attributes of the metadata
#' 
#' @examples
#' dF<- data.frame(id=c("A","B","3D"),
#'                 name=c("tom","andy","ellie"),
#'                 dob=c("1984-03-14","1985-11-13","1987-06-16"),
#'                 gender=c("male","male","female"),
#'                 rec1contact=c(2,1,5),
#'                 rec1date=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"))
#' 
#' metadata1<-createMetadata (attributes=list(createAttribute(name="name",type="str",value=list(dF$name)),
#'                                            createAttribute(name="dob",type="date",value=list(as.Date(dF$dob))),
#'                                            createAttribute(name="gender",type="str",value=list(dF$gender)),
#'                                            createAttribute(name="rec1risk",type="str",value=list(dF$rec1risk)),
#'                                            createAttribute(name="rec1temp",type="int",value=list(dF$rec1temp)),
#'                                            createAttribute(name="rec2risk",type="str",value=list(dF$rec2risk))  ))
#'                                            
#'                              
#'               
#' @return an \code{\link{ejMetadata}} object
#' @export
createMetadata <- function(attributes){
	structure(attributes, class="ejMetadata")
}