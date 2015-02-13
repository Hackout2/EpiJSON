#This set of functions prints ej'Objects' to more friendly form
 
#' print an ejAttribute object
#' 
#' @param x An ejAttribute object
#' @param ... Other arguments to print (not used)
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
#' x <- createAttribute(name="name",type="string",value=dF$name[1])
#' print.ejAttribute(x)
#' 
#' @export 
print.ejAttribute <- function(x, ...){
	cat("(name: ", x$name, " type:", x$type, " value:", x$value, ")\n")
}

#' print an ejEvent object
#' 
#' @param x An ejEvent object
#' @param ... Other arguments to print (not used)
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
#' x <- createEvent(id=NA, 
#'              name="rec1contact",
#'              dateStart=as.POSIXct(dF$rec1dateStart[1]),
#'              dateEnd=as.POSIXct(dF$rec1dateEnd[1]),
#'              location=NA,
#'              attributes=list(createAttribute(name="rec1risk",type="str",value=c(as.character(dF$rec1risk[1]))),
#'                              createAttribute(name="rec1temp",type="int",value=c(as.character(dF$rec1temp[1])))))
#'
#' print.ejEvent(x)
#' 
#' @export  
print.ejEvent <- function(x, ...){
	cat("event:\n")
	cat("id: ", x$id, "\n")
	cat("name:", x$name, "\n")
	cat("dateStart: ", x$dateStart, "\n")
	cat("dateEnd: ", x$dateEnd, "\n")
	#cat("location: ", coordinates(x$location)[1], ", ", coordinates(x$location)[2], "\n")
	for(attribute in x$attributes){print.ejAttribute(attribute)}
}

#' print an ejRecord object
#' 
#' @param x An ejRecord object
#' @param ... Other arguments to print (not used)
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
#' 
#' x <- createrecord(id=dF$id[1], 
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
#'
#' print.ejRecord(x)
#' 
#' @export 
print.ejRecord <- function(x, ...){
	cat("record:\n")
	cat("id:", x$id,"\n")
	for(attribute in x$attributes){print.ejAttribute(attribute)}
	for(event in x$events){print.ejEvent(event)}
}

#' print an ejMetadata object
#' 
#' @param x An ejMetadata object
#' @param ... Other arguments to print (not used)
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
#' 
#' x <- createMetadata (attributes=list(createAttribute(name="name",type="str",value=dF$name),
#'                                            createAttribute(name="dob",type="date",value=dF$dob),
#'                                            createAttribute(name="gender",type="str",value=dF$gender),
#'                                            createAttribute(name="rec1risk",type="str",value=dF$rec1risk),
#'                                            createAttribute(name="rec1temp",type="int",value=dF$rec1temp),
#'                                            createAttribute(name="rec2risk",type="str",value=dF$rec2risk)))
#'                      
#' print.ejMetadata(x)
#' 
#' @export 
print.ejMetadata <- function(x,...){
	cat("MetaData:\n")
	for(attribute in x){print.ejAttribute(attribute)}
}

#' print an ejObject object
#' 
#' @param x An ejObject object
#' @param ... Other arguments to print (not used)
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
#' x <- createEJObject (metadata=list(list(name="ID",type="str",value=dF$id[1]),
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
#' print.ejObject(x)
#' 
#' @export 
print.ejObject <- function(x, ...){
	cat("EpiJSON object\n")
	print.ejMetadata(x$metadata)
	for(record in x$records){print.ejRecord(record)}
}
