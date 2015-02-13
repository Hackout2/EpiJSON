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
#' dF<- data.frame(id=c("A","B","3D"),
#'                 name=c("tom","andy","ellie"),
#'                 dob=c("1984-03-14","1985-11-13","1987-06-16"),
#'                 gender=c("male","male","female"),
#'                 rec1contact=c(2,1,5),
#'                 rec1dateStart=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1dateEnd=c("2014-12-30","2015-01-04","2015-01-07"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"),stringsAsFactors=FALSE)
#' 
#' createAttribute(name="name",type="int",value=dF$name[1])
#' 
#' @return an ejAttribute object
#' @export
createAttribute <- function (name, type, value){
	structure(list(
					name=name,
					type=type,
					value=value
			), class="ejAttribute")
}

#' Create an event
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
#'                 rec1dateStart=c(as.POSIXct(tz="GMT","2014-12-28"),as.POSIXct(tz="GMT","2014-12-29"),as.POSIXct(tz="GMT","2015-01-03")),
#'                 rec1dateEnd=c(as.POSIXct(tz="GMT","2014-12-30"),as.POSIXct(tz="GMT","2015-01-04"),as.POSIXct(tz="GMT","2015-01-09")),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2date=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"),stringsAsFactors=FALSE)
#'                 
#' event1<-createEvent(id=NA, 
#'                       name="rec1contact",
#'                       dateStart=dF$rec1dateStart[1],
#'                       dateEnd=dF$rec1dateEnd[1],
#'                       location="",
#'                       attributes=list(createAttribute(name="rec1risk",type="str",value=dF$rec1risk[1]),
#'                              createAttribute(name="rec1temp",type="int",value=dF$rec1temp[1])))
#' 
#' @return an ejEvent object
#' @export
createEvent <- function(id=NA, name, dateStart, dateEnd, location, attributes){
	structure(list( 
					id=id,
					name=name,
					dateStart=strftime(dateStart, "%Y-%m-%dT%H:%M:%S%z"),
					dateEnd=strftime(dateEnd, "%Y-%m-%dT%H:%M:%S%z"),
					location=location,
					attributes=attributes
			), class="ejEvent")
}

#' Create a record
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
#'                 rec1dateStart=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1dateEnd=c("2014-12-30","2015-01-04","2015-01-07"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2dateStart=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"),stringsAsFactors=FALSE)
#' 
#' record1<-createrecord(id=dF$id[1], 
#'              attributes=list(createAttribute(name="name",type="str",value=dF$name[1]),
#'                              createAttribute(name="dob",type="date",value=dF$dob[1]),
#'                              createAttribute(name="gender",type="str",value=dF$gender[1])),
#'              events=list(createEvent(id=NA, 
#'                                        name="rec1contact",
#'                                        dateStart=as.POSIXct(dF$rec1dateStart[1]),
#'                                        dateEnd=as.POSIXct(dF$rec1dateEnd[1]),
#'                                        location="",
#'                                        attributes=list(createAttribute(name="rec1risk",type="str",value=dF$rec1risk[1]),
#'                                                        createAttribute(name="rec1temp",type="int",value=dF$rec1temp[1])
#'                                                                  )),
#'                          createEvent(id=NA, 
#'                                       name="rec2contact",
#'                                       dateStart=dF$rec2dateStart[1],
#'                                       dateEnd=dF$rec2dateStart[1],
#'                                       location="",
#'                                              attributes=list(createAttribute(name="rec2risk",type="str",value=dF$rec2risk[1]))
#'                                        )
#'                              )  
#'                      )     
#'               
#' @return an ejEvent object
#' @export
#' 
createrecord <- function(id, attributes, events){
	structure(list(
					id=id,
					attributes=attributes,
					events=events
					), class="ejRecord")
}


#' Create an object
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
#'                 rec1dateStart=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1dateEnd=c("2014-12-30","2015-01-04","2015-01-07"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2dateStart=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"),stringsAsFactors=FALSE)
#' 
#' obj1<-createEJObject (metadata=list(list(name="ID",type="str",value=dF$id[1]),
#'                                     list(name="name",type="str",value=dF$name[1]),
#'                                     list(name="rec1contact",type="str",value=dF$rec1contact[1]),
#'                                     list(name="rec1date",type="date",value=dF$rec1date[1])),
#'                                                  records=list(createrecord(id=dF$id[1], 
#'                                                                            attributes=list(createAttribute(name="name",type="str",value=dF$name[1]),
#'                                                                                            createAttribute(name="dob",type="date",value=dF$dob[1]),
#'                                                                                            createAttribute(name="gender",type="str",value=dF$gender[1])),
#'                                                                            events=list(createEvent(id=NA, 
#'                                                                                                    name="rec1contact",
#'                                                                                                    dateStart=as.POSIXct(dF$rec1dateStart[1]),
#'                                                                                                    dateEnd=as.POSIXct(dF$rec1dateEnd[1]),
#'                                                                                                    location="",
#'                                                                                                    attributes=list(createAttribute(name="rec1risk",type="str",value=dF$rec1risk[1]),
#'                                                                                                                    createAttribute(name="rec1temp",type="int",value=dF$rec1temp[1])
#'                                                                                                                 )),
#'                                                                                        createEvent(id=NA, 
#'                                                                                                    name="rec2contact",
#'                                                                                                    dateStart=dF$rec2dateStart[1],
#'                                                                                                    dateEnd=dF$rec2dateStart[1],
#'                                                                                                    location="",
#'                                                                                                    attributes=list(createAttribute(name="rec2risk",type="str",value=dF$rec2risk[1]))
#'                                                                                                      )
#'                                                                                                )  
#'                                                                                          )
#'                                                                                    )
#'                                                                              ) 
#'                   
#'               
#' @return an ejObject object
#' @export
createEJObject <- function(metadata, records){
	structure(list(
					metadata=metadata,
					records=records
	), class="ejObject")
}

#' Create metadata
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
#'                 rec1dateStart=c("2014-12-28","2014-12-29","2015-01-03"),
#'                 rec1dateEnd=c("2014-12-30","2015-01-04","2015-01-07"),
#'                 rec1risk=c("high","high","low"),  
#'                 rec1temp=c(39,41,41),
#'                 rec2contact=c(4,1,1),
#'                 rec2dateStart=c("2015-01-02","2015-01-12","2015-01-09"),
#'                 rec2risk=c("high","low","high"),stringsAsFactors=FALSE)
#' 
#' metadata1<-createMetadata (attributes=list(createAttribute(name="name",type="str",value=dF$name),
#'                                            createAttribute(name="dob",type="date",value=dF$dob),
#'                                            createAttribute(name="gender",type="str",value=dF$gender),
#'                                            createAttribute(name="rec1risk",type="str",value=dF$rec1risk),
#'                                            createAttribute(name="rec1temp",type="int",value=dF$rec1temp),
#'                                            createAttribute(name="rec2risk",type="str",value=dF$rec2risk)))
#'                                            
#'                              
#'               
#' @return an ejMetadata object
#' @export
createMetadata <- function(attributes){
	structure(attributes, class="ejMetadata")
}
