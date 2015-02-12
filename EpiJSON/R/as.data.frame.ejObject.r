

#' convert an \code{\link{ejObject}} to a dataframe
#' 
#' convert an \code{\link{ejObject}} (a list of lists) to a dataframe with one row per individual


#' @param ejOb an \code{\link{ejObject}}
#' 
#' @return dataframe
#' @examples
#' 
#' att <- createAttribute(name="name",type="int",value=7)
#' atts <- list(att)
#' record <- createRecord(id=NA, name="rec1", date=NA, location=NA, attributes=atts )
#' individuals <- list(createIndividual(id="bob", attributes=atts, records=list(record)))
#' ind1 <- createIndividual(id="bob", attributes=atts, records=list(record))
#' ind2 <- createIndividual(id="pat", attributes=atts, records=list(record))
#' individuals <- list(ind1,ind2)
#' ejOb <- createEJObject(metadata=atts, individuals=individuals)
#' 
#' #ejOb <- createEJObject(metadata=NULL, individuals=NULL)
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
#'  ejOb2 <- as.ejObject(simulated, individualAttributes = c("gender"),
#'   	recordDefinitions = list(defineEJRecord(date="date", name=NA, location=list(x="x", y="y", proj4string=""), attributes="pump")),
#' 		metadata=list())
#'  as.data.frame.ejObject(ejOb2)
#'      
#' @export
#' 
as.data.frame.ejObject <- function(ejOb){
  
  #not sure whether we can put metadata into the df
  #ejOb$metadata
  
  #individuals
  #want to create one row for each individual
  indivs <- ejOb$individuals
  #BUT need to go for each individual, 
  
  #add row to dF
  #for each record name
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
    
    #class ejIndividual
    indiv <- indivs[[iNum]]
    

    #set id for this individual
    dF$id[iNum] <- indiv$id
    
    #to do I will also need to get the attributes of the individuals
    #and put them into columns
    for( aNum in 1:length(indiv$attributes))
    {
      att <- indiv$attributes[[aNum]]
      #browser()
      dF <- findOrAdd(dF, name=att$name, rowNum=iNum, value=att$value)      
    }
    
    
    records <- indiv$records
    for( recNum in 1:length(records))
    {
      #class ejRecord
      record <- records[[recNum]]
      
      #first get record name, date and location
      #name date and location columns by pasting on recordName
      #will need a function that accepts a dataframe, and a name
      #if the name is already a column name, use it
      #otherwise add a new column
      nameDate <- paste("date",dF[[record$name]])
      nameX <- paste("x",dF[[record$name]]) 
      nameY <- paste("y",dF[[record$name]]) 
      nameCRS <- paste("CRS",dF[[record$name]]) 
      
      dF <- findOrAdd(dF, name=nameDate, rowNum=iNum, value=record$date)
      #todo this will need to get x,y,CRS from location
      #dF <- findOrAdd(dF, name=nameX, rowNum=iNum, value=record$location)
      

    }
    
    
  }
  
  
#   blank <- NULL
#   tst <- lapply(ejOb$individuals, function(x) blank <- rbind(blank,unlist(x))
  
  
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
