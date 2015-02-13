#' This set of functions converts the outputs from the ej'Objects' to more user-friendly form
#' 

#' Attributes
#' @example
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
#' x <- createAttribute(name="name",type="string",value=c(as.character(dF$name[1])))
#' print.ejAttribute(x)
#' 
#' @export 

print.ejAttribute <- function(x, ...){
	cat("(name: ", x$name, " type:", x$type, " value:", x$value, ")\n")
}

#' events
#' @example
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
#' x <- createevent(id=NA, 
#'              name="rec1contact",
#'              date=as.POSIXct(dF$rec1date[1]),
#'              location=NA,
#'              attributes=list(createAttribute(name="rec1risk",type="str",value=c(as.character(dF$rec1risk[1]))),
#'                              createAttribute(name="rec1temp",type="int",value=c(as.character(dF$rec1temp[1])))))
#'
#' print.ejEvent(x)
#' 
#' @export 
#' 
print.ejEvent <- function(x, ...){
	cat("event:\n")
	cat("id: ", x$id, "\n")
	cat("name:", x$name, "\n")
	cat("date: ", x$date, "\n")
	#cat("location: ", coordinates(x$location)[1], ", ", coordinates(x$location)[2], "\n")
	for(attribute in x$attributes){print.ejAttribute(attribute)}
}

#' record
#' @example
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
#' x <- createrecord(id=dF$id[1], 
#'              attributes=list(createAttribute(name="name",type="str",value=c(as.character(dF$name[1]))),
#'                              createAttribute(name="gender",type="str",value=c(as.character(dF$gender[1])))),
#'              events=list(createevent(id=NA, 
#'                                        name="rec1contact",
#'                                        date=as.POSIXct(dF$rec1date[1]),
#'                                        location="",
#'                                              attributes=list(createAttribute(name="rec1risk",type="str",value=c(as.character(dF$rec1risk[1]))),
#'                                                              createAttribute(name="rec1temp",type="int",value=dF$rec1temp[1]))
#'                                                                  ),
#'                          createevent(id=NA, 
#'                                       name="rec2contact",
#'                                       date=as.POSIXct(dF$rec2date[1]),
#'                                       location="",
#'                                              attributes=list(createAttribute(name="rec2risk",type="str",value=dF$rec2risk[1])))
#'                                        )
#'                              )  
#'                    
#'
#' print.ejRecord(x)
#' 
#' @export 
#'  
print.ejRecord <- function(x, ...){
	cat("record:\n")
	cat("id:", x$id,"\n")
	for(attribute in x$attributes){print.ejAttribute(attribute)}
	for(event in x$events){print.ejEvent(event)}
}

#' Metadata
#' @example
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
#' x <- createMetadata (attributes=list(createAttribute(name="name",type="str",value=as.character(dF$name[1])),
#'                                            createAttribute(name="dob",type="date",value=as.POSIXct(dF$dob[1])),
#'                                            createAttribute(name="gender",type="str",value=as.character(dF$gender[1])),
#'                                            createAttribute(name="rec1risk",type="str",value=as.character(dF$rec1risk[1])),
#'                                            createAttribute(name="rec1temp",type="int",value=dF$rec1temp[1]),
#'                                            createAttribute(name="rec2risk",type="str",value=as.character(dF$rec2risk[1]))  ))
#'
#' print.ejMetadata(x)
#' 
#' @export 
#' 

print.ejMetadata <- function(x,...){
	cat("MetaData:\n")
	for(attribute in x){print.ejAttribute(attribute)}
}

#' Object
#' @example
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
#' x <- createEJObject (metadata=list(createAttribute(name="name",type="str",value=as.character(dF$name[1])),
#'                                            createAttribute(name="dob",type="date",value=as.POSIXct(dF$dob[1])),
#'                                            createAttribute(name="gender",type="str",value=as.character(dF$gender[1])),
#'                                            createAttribute(name="rec1risk",type="str",value=as.character(dF$rec1risk[1])),
#'                                            createAttribute(name="rec1temp",type="int",value=dF$rec1temp[1]),
#'                                            createAttribute(name="rec2risk",type="str",value=as.character(dF$rec2risk[1]))  ),
#'                                                  records=list(createrecord(id=as.character(dF$id[1]), 
#'              attributes=list(createAttribute(name="name",type="str",value=c(as.character(dF$name[1]))),
#'                              createAttribute(name="gender",type="str",value=c(as.character(dF$gender[1])))),
#'              events=list(createevent(id=NA, 
#'                                        name="rec1contact",
#'                                        date=as.POSIXct(dF$rec1date[1]),
#'                                        location="",
#'                                              attributes=list(createAttribute(name="rec1risk",type="str",value=c(as.character(dF$rec1risk[1]))),
#'                                                              createAttribute(name="rec1temp",type="int",value=dF$rec1temp[1]))
#'                                                                  ),
#'                          createevent(id=NA, 
#'                                       name="rec2contact",
#'                                       date=as.POSIXct(dF$rec2date[1]),
#'                                       location="",
#'                                              attributes=list(createAttribute(name="rec2risk",type="str",value=dF$rec2risk[1])))
#'                                        )
#'                              )) ) 
#'                                                                            

#' print.ejObject(x)
#' 
#' @export 
#' 
print.ejObject <- function(x, ...){
	cat("EpiJSON object\n")
	print.ejMetadata(x$metadata)
	for(record in x$records){print.ejRecord(record)}
}