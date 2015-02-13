#' convert an \code{\link{ejObject}} to a dataframe with one row per record

#' @param ejOb an \code{\link{ejObject}}
#' 
#' @return dataframe
#' @examples
#' 
#' att <- createAttribute(name="name",type="int",value=7)
#' atts <- list(att)
#' event <- createEvent(id=NA, name="event1", dateStart=NA, dateEnd=NA, attributes=atts )
#' #a single record
#' #records <- list(createRecord(id="bob", attributes=atts, events=list(event)))
#' ind1 <- createRecord(id="bob", attributes=atts, events=list(event))
#' ind2 <- createRecord(id="pat", attributes=atts, events=list(event))
#' records <- list(ind1,ind2)
#' ejOb <- createEJObject(metadata=atts, records=records)
#' 
#' #ejOb <- createEJObject(metadata=NULL, records=NULL)
#' as.data.frame(ejOb)
#' 
#'  #see eg from as.ejObject.data.frame for more complex example
#'  library(HistData)
#'  data(Snow.deaths)
#'  #It would be good to have some dates, pumps, some genders 
#'  simulated <- Snow.deaths
#'  simulated$gender <- c("male","female")[(runif(nrow(simulated))>0.5) +1]
#'  simulated$dateStart <- as.POSIXct("1854-04-05") + rnorm(nrow(simulated), 10) * 86400
#'  simulated$dateEnd <- as.POSIXct("1854-04-05") + rnorm(nrow(simulated), 10) * 86400  
#'  simulated$pump <- ceiling(runif(nrow(simulated)) * 5)
#'  ejOb2 <- as.ejObject(simulated, recordAttributes = c("gender"),
#'   	eventDefinitions = list(defineEjEvent(dateStart="dateStart", dateEnd="dateEnd", name=NA, location=list(x="x", y="y", proj4string=""), attributes="pump")),
#' 		metadata=list())
#'  as.data.frame(ejOb2)
#'      
#' @export
#' 
as.data.frame.ejObject <- function(ejOb){
  
  #not sure whether we can put metadata into the df
  #ejOb$metadata
  
  #want to create one row for each record
  records <- ejOb$records
  
  #create blank dataFrame with a single column to start
  dF <- data.frame(id=rep(NA,length(records)))
  
  for( iNum in 1:length(records))
  {
    #cat(iNum)
    
    #class ejRecord
    record <- records[[iNum]]
    

    #set id for this record
    dF$id[iNum] <- record$id
    
    #to do I will also need to get the attributes of the records
    #and put them into columns
    for( aNum in 1:length(record$attributes))
    {
      att <- record$attributes[[aNum]]
      #browser()
      dF <- findOrAdd(dF, name=att$name, rowNum=iNum, value=att$value)      
    }    
    
    events <- record$events
    for( eventNum in 1:length(events))
    {
      #class ejEvent
      event <- events[[eventNum]]
      
      #get event name, date and location
      #name date and location columns by pasting on eventName
      nameDateStart <- paste("dateStart",dF[[event$name]])
      nameDateEnd <- paste("dateEnd",dF[[event$name]])
      nameX <- paste("x",dF[[event$name]]) 
      nameY <- paste("y",dF[[event$name]]) 
      nameCRS <- paste("CRS",dF[[event$name]]) 

      #if the name is already a column name, use it otherwise add a new column
      dF <- findOrAdd(dF, name=nameDateStart, rowNum=iNum, value=event$dateStart)
      dF <- findOrAdd(dF, name=nameDateEnd, rowNum=iNum, value=event$dateEnd)
      #get x,y,CRS from location
      if(class(event$location) == "SpatialPoints"){
        dF <- findOrAdd(dF, name=nameX, rowNum=iNum, value=event$location$x)
        dF <- findOrAdd(dF, name=nameY, rowNum=iNum, value=event$location$y)
        dF <- findOrAdd(dF, name=nameCRS, rowNum=iNum, value=proj4string(event$location))
      }
      
      #for attributes of events
      #todo put this into a function, called above as well
      #I don't know why the functio gives the error
      #In `[<-.data.frame`(`*tmp*`, name, value = list(pump = c(NA, NA,  :
      #provided 2 variables to replace 1 variables
      #dF <- findOrAddAttributes(dF, atts=record$attributes)      
      for( aNum in 1:length(event$attributes))
      {
        att <- event$attributes[[aNum]]
        #browser()
        dF <- findOrAdd(dF, name=att$name, rowNum=iNum, value=att$value)      
      }
    } 
  } 
  return(dF)
}


#'helper func to go through all attributes and add them to a dataframe
#'
#'DOESN'T WORK at the moment, so not called
#'adds 'new' attributes to a new column, existing attributes to existing column
#' @param dF a dataframe
#' @param atts an ejAttributes object
findOrAddAttributes <- function(dF, atts)
{
  for( aNum in 1:length(atts))
  {
    att <- atts[[aNum]]
    #browser()
    dF <- findOrAdd(dF, name=att$name, rowNum=iNum, value=att$value)      
  }
  
  return(dF)
}

#'helper func to find a column name in a dataframe or add a new one
#'
#'Checks for 'name' as a column name in the dataframe, if it is found the value is put there.  
#'If the name is not found a new column is created and the value is put in the new column.
#' @param dF a dataframe
#' @param name a column name to search for in the dataframe
#' @param rowNum which row to add the value to
#' @param value the value to put in the dataframe at the chosen row and column

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
