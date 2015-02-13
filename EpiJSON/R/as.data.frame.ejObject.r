

#' convert an \code{\link{ejObject}} to a dataframe
#' 
#' convert an \code{\link{ejObject}} (a list of lists) to a dataframe with one row per record


#' @param ejOb an \code{\link{ejObject}}
#' 
#' @return dataframe
#' @examples
#' 
#' att <- createAttribute(name="name",type="int",value=7)
#' atts <- list(att)
#' event <- createevent(id=NA, name="rec1", date=NA, location=NA, attributes=atts )
#' records <- list(createrecord(id="bob", attributes=atts, events=list(event)))
#' ind1 <- createrecord(id="bob", attributes=atts, events=list(event))
#' ind2 <- createrecord(id="pat", attributes=atts, events=list(event))
#' records <- list(ind1,ind2)
#' ejOb <- createEJObject(metadata=atts, records=records)
#' 
#' #ejOb <- createEJObject(metadata=NULL, records=NULL)
#' as.data.frame.ejObject(ejOb)
#' 
#'  #see eg from as.ejObject.data.frame for more complex example
#'  library(HistData)
#'  data(Snow.deaths)
#'  #It would be good to have some dates, pumps, some genders 
#'  simulated <- Snow.deaths
#'  simulated$gender <- c("male","female")[(runif(nrow(simulated))>0.5) +1]
#'  simulated$date <- as.POSIXct("1854-04-05") + rnorm(nrow(simulated), 10) * 86400
#'  simulated$pump <- ceiling(runif(nrow(simulated)) * 5)
#'  ejOb2 <- as.ejObject(simulated, recordAttributes = c("gender"),
#'   	eventDefinitions = list(defineejEvent(date="date", name=NA, location=list(x="x", y="y", proj4string=""), attributes="pump")),
#' 		metadata=list())
#'  as.data.frame.ejObject(ejOb2)
#'      
#' @export
#' 
as.data.frame.ejObject <- function(ejOb){
  
  #not sure whether we can put metadata into the df
  #ejOb$metadata
  
  #records
  #want to create one row for each record
  indivs <- ejOb$records
  #BUT need to go for each record, 
  
  #add row to dF
  #for each event name
  #if name is new
  #  add column to dF, named name
  #  add value at row,col
  #else
  #  
  #add value at row,col
  
  #create blank dataFrame with a single column to start
  dF <- data.frame(id=rep(NA,length(indivs)))
  
  for( iNum in 1:length(indivs))
  {
    #cat(iNum)
    
    #class ejRecord
    indiv <- indivs[[iNum]]
    

    #set id for this record
    dF$id[iNum] <- indiv$id
    
    #to do I will also need to get the attributes of the records
    #and put them into columns
    for( aNum in 1:length(indiv$attributes))
    {
      att <- indiv$attributes[[aNum]]
      #browser()
      dF <- findOrAdd(dF, name=att$name, rowNum=iNum, value=att$value)      
    }
    
    
    events <- indiv$events
    for( recNum in 1:length(events))
    {
      #class ejEvent
      event <- events[[recNum]]
      
      #first get event name, date and location
      #name date and location columns by pasting on eventName
      #will need a function that accepts a dataframe, and a name
      #if the name is already a column name, use it
      #otherwise add a new column
      nameDate <- paste("date",dF[[event$name]])
      nameX <- paste("x",dF[[event$name]]) 
      nameY <- paste("y",dF[[event$name]]) 
      nameCRS <- paste("CRS",dF[[event$name]]) 
      
      dF <- findOrAdd(dF, name=nameDate, rowNum=iNum, value=event$date)
      #todo this will need to get x,y,CRS from location
      #dF <- findOrAdd(dF, name=nameX, rowNum=iNum, value=event$location)
      

    }
    
    
  }
  
  
#   blank <- NULL
#   tst <- lapply(ejOb$records, function(x) blank <- rbind(blank,unlist(x))
  
  
  return(dF)

}

#'helper func to find a col name in a dataframe or add a new one

findOrAdd <- function(dF, name, rowNum, value)
{
  colNum <- which( names(dF)==name )
  if (length(colNum)==0)
  {
    #add a column 
    dF[name] <- NA
    #and insert the value
    dF[name][rowNum] <- value
    
  } else if (length(colNum)>1)
  {
    stop("repeated column names")
  } else
  {
    #insert the value
    dF[rowNum,colNum] <- value
  }
  
  return(dF)
}
