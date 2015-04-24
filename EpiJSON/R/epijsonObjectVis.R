#' to view the structure of epijson objects and/or schema

#' testing ability/utility
#' structure is largely hardcoded for now, could try to automate later

#' @return a ggplot object
#' @export
epijsonObjectVis <- function(){

  #todo : this could be refactored with pipes %>%
  
  gg <- box2(xmin=0.01, xmax=0.99, ymin=0.01, ymax=0.99, label='EpiJSON file')
  gg <- box2(xmin=0.05, xmax=0.95, ymin=0.70, ymax=0.90, label='Metadata', gg=gg)
  gg <- box2(xmin=0.05, xmax=0.95, ymin=0.04, ymax=0.67, label='Records', gg=gg)
  gg <- box2(xmin=0.10, xmax=0.88, ymin=0.10, ymax=0.60, label='Record [id]', gg=gg, addSheets=2)
  
  gg <- box2(xmin=0.09, xmax=0.69, ymin=0.75, ymax=0.84, label='Attribute [name, type, value, units]', gg=gg, addSheets=2)
  gg <- box2(xmin=0.14, xmax=0.74, ymin=0.45, ymax=0.54, label='Attribute [name, type, value, units]', gg=gg, addSheets=2)
  
  gg <- box2(xmin=0.14, xmax=0.82, ymin=0.15, ymax=0.40, label='Event [id, name, date, location]', gg=gg, addSheets=2)
  gg <- box2(xmin=0.18, xmax=0.78, ymin=0.20, ymax=0.30, label='Attribute [name, type, value, units]', gg=gg, addSheets=2)
  
  print(gg) 
  
  invisible(gg)
}

#' 2 view the structure of epijson objects and/or schema

#' starting to automate, making box positions relative

#' @return a ggplot object
#' @export
epijsonObjectVis2 <- function(){
  
  #todo : this could be refactored with pipes %>%
  
  #first get the generic schema version to work by tweaking coords as go along
  #this will then make flexibl;e to change num objects in containers
  
  xatt <- 0.60 #length att box
  yatt <- 0.06 #height att box
  
  #here ymin set to 0.01 later can be set ac to num objects & can be set to < 1 to make plot bigger
  xmin=0.01; xmax=0.99; ymin=0.01; ymax=0.99
  
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='EpiJSON file')
  
  #because working down, set ymax first, then ymin from ymax (could later use num objects to set ymin)
  xmin=xmin+0.04; xmax=xmax-0.04; ymax=ymax-0.09; ymin=ymax-0.20; 
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Metadata', gg=gg)
  #saving metadata dims to use later
  mxmin=xmin; mxmax=xmax; mymin=ymin; mymax=ymax
  
  #now set ymax from ymin of previous
  xmin=xmin; xmax=xmax; ymax=ymin-0.03; ymin=ymax-0.65 #later ymin can be set ac to num objects   
  
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Records', gg=gg)
  
  #record is simpler because it just fits in records
  xmin=xmin+0.05; xmax=xmax-0.05; ymax=ymax-0.05; ymin=ymin+0.04;    
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Record [id]', gg=gg, addSheets=2, colour="blue")
  #saving record dims to use later
  rxmin=xmin; rxmax=xmax; rymin=ymin; rymax=ymax
  
  #todo
  #Aha! now that I've got to attributes which need to go in other containers gets trickier
  #but maybe I can hardcode the first & add others if needed
  #or I can save the xmin etc for metadata,record, and events
  
  #Metadata attributes
  xmin=mxmin+0.05; xmax=xmin+xatt; ymax=mymax-0.06; ymin=ymax-yatt;    
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Attribute [name, type, value, units]', gg=gg, addSheets=2, colour="purple")
  #gg <- box2(xmin=0.09, xmax=0.69, ymin=0.75, ymax=0.84, label='Attribute [name, type, value, units]', gg=gg, addSheets=2, colour="purple")
  #Record attributes
  xmin=rxmin+0.05; xmax=xmin+xatt; ymax=rymax-0.06; ymin=ymax-yatt; 
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Attribute [name, type, value, units]', gg=gg, addSheets=2, colour="purple")
  #gg <- box2(xmin=0.14, xmax=0.74, ymin=0.45, ymax=0.54, label='Attribute [name, type, value, units]', gg=gg, addSheets=2, colour="purple")
  
  #Event & Event attributes
  #Event needs to go below the last attribute of Record
  xmin=rxmin+0.05; xmax=rxmax-0.05; ymax=ymin-0.04; ymin=ymax-(yatt+0.2) #later base event y on numatts    
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Event [id, name, date, location]', gg=gg, addSheets=2, colour="red")  
  #gg <- box2(xmin=0.14, xmax=0.82, ymin=0.15, ymax=0.40, label='Event [id, name, date, location]', gg=gg, addSheets=2, colour="red")
  
  xmin=xmin+0.05; xmax=xmin+xatt; ymax=ymax-0.06; ymin=ymax-yatt;  
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Attribute [name, type, value, units]', gg=gg, addSheets=2, colour="purple")
  #gg <- box2(xmin=0.18, xmax=0.78, ymin=0.20, ymax=0.30, label='Attribute [name, type, value, units]', gg=gg, addSheets=2, colour="purple")
  
  print(gg) 
  
  invisible(gg)
}


#' 3 view the structure of epijson objects and/or schema

#' allowing multiple attribute boxes with different labels

#' @param attributeMeta label(s) for Metadata attributes
#' @param attributeRecord label(s) for Record attributes
#' @param attributeEvent label(s) for Event attributes
#' 
#' @return a ggplot object
#' @examples 
#' #this gives the base schema
#' epijsonObjectVis3()
#' #this gives a diagram with named attributes
#' epijsonObjectVis3( attribMeta = c("attribute: disease","attribute: data provider"),
#'                    attribRecord = c("attribute: gender","attribute: date of birth"),
#'                    attribEvent = c("attribute: recorder","attribute: test used") )
#' epijsonObjectVis3( attribMeta = c("a"),
#'                    attribRecord = c("b","c"),
#'                    attribEvent = c("d","e","f") )
#' @export
epijsonObjectVis3 <- function( attribMeta = 'Attribute [name, type, value, units]',
                               attribRecord = 'Attribute [name, type, value, units]',
                               attribEvent = 'Attribute [name, type, value, units]' )
  
{
  
  
  #count number of attributes
  nattM <- length(attribMeta)
  nattR <- length(attribRecord)  
  nattE <- length(attribEvent)
  #todo sort this
  #assume that if num atts is 1, show multiple sheets, otherwise just show indiv sheets
  #(this gives misleading pic if there is indeed just one attribute)
  sheetsAttM <- sheetsAttR <- sheetsAttE <- 2
  if (nattM>1) sheetsAttM <- 0 
  if (nattR>1) sheetsAttR <- 0   
  if (nattE>1) sheetsAttE <- 0 
  
  
  xatt <- 0.60 #length att box
  yatt <- 0.06 #height att box (excl 'sheets')
  ysheets <- xsheets <- 0.02
  #could us these spacings below too 
  xgap <- 0.04
  ygap <- 0.02
  #todo below it's not quite consistent whether ylab includes gap or not
  ylab <- 0.04  #spacing for label text, includes gap could base on font size
  
  #spacing for containers
  yM <- ylab + nattM*(yatt+ygap) + sheetsAttM*ysheets 
  yE <- ylab + nattE*(yatt+ygap) + sheetsAttE*ysheets #+ ygap
  #yR for Record (not records)
  yR <- ylab + nattR*(yatt+ygap) + sheetsAttR*ysheets + yE + ysheets + ygap + ygap #todo this last ygap is a fudge because addSheets is 2 when it should be 1 for sizing  
  yRs <- ylab+yR+ygap+ysheets+ygap
  
  yAll <- ylab + ygap + yM + ygap + yRs + ygap
  
  #cat("yatt:",yatt," yM:",yM," yE:",yE,"yR:",yR,"\n")
  
  #ymax set to total height of plot
  xmin=0.01; xmax=0.99; ymin=0.01; ymax=ymin+yAll 
  
  #todo : gg use could be refactored with pipes %>%
  
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='EpiJSON file')
  
  ## Metadata
  #work down, set ymax first, then ymin from ymax
  xmin=xmin+xgap; xmax=xmax-xgap; ymax=ymax-(ylab+ygap); ymin=ymax-yM; 
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Metadata', gg=gg)
  #saving metadata dims to use later
  mxmin=xmin; mxmax=xmax; mymin=ymin; mymax=ymax
  
  ##Records
  #set ymax from ymin of Metadata
  xmin=xmin; xmax=xmax; ymax=ymin-ygap; ymin=ymax-yRs   
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Records', gg=gg)
  
  ##Record
  #record is simpler because it just fits in records
  xmin=xmin+xgap; xmax=xmax-xgap; ymax=ymax-(ylab+ygap); ymin=ymin+(ygap+ysheets);    
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Record [id]', gg=gg, addSheets=2, colour="blue")
  #saving record dims to use later
  rxmin=xmin; rxmax=xmax; rymin=ymin; rymax=ymax
  
  
  #Metadata attributes
  for(i in 1:nattM)
  {
    xmin=mxmin+xgap; xmax=xmin+xatt; ymax=mymax-i*(yatt); ymin=ymax-yatt;    
    gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label=attribMeta[i], gg=gg, addSheets=sheetsAttM, colour="purple")
  }
  #Record attributes
  for(i in 1:nattR)
  {
    xmin=rxmin+xgap; xmax=xmin+xatt; ymax=rymax-i*(yatt); ymin=ymax-yatt; 
    gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label=attribRecord[i], gg=gg, addSheets=sheetsAttR, colour="purple")
  }
  
  #Event needs to go below the last attribute of Record
  xmin=rxmin+xgap; xmax=rxmax-(xgap+xsheets); ymax=ymin-ygap-(sheetsAttR*ysheets); ymin=ymax-yE    
  gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label='Event [id, name, date, location]', gg=gg, addSheets=2, colour="red")  
  #saving event dims to use later
  exmin=xmin; exmax=xmax; eymin=ymin; eymax=ymax
  
  #Event attributes
  for(i in 1:nattE)
  {
    xmin=exmin+xgap; xmax=xmin+xatt; ymax=eymax-i*(yatt); ymin=ymax-yatt;  
    gg <- box2(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label=attribEvent[i], gg=gg, addSheets=sheetsAttE, colour="purple")
  }
  
  print(gg) 
  
  invisible(gg)
}


#' generic labelled box function

#' @param xmin 0 to 1
#' @param xmax 0 to 1   
#' @param ymin 0 to 1 
#' @param ymax 0 to 1 
#' @param label to print at top of box
#' @param colour border of box
#' @param fill fill colour for box NA=none
#' @param gg ggplot object, if passed box is added if NULL new ggplot object created
#' @param addSheets how many sheets to add behind box to indicate array
#' @param size font size for label
#' @param print TRUE/FALSE whether to print the ggplot object
#' 
#' @return a ggplot object
#' 
box2 <- function(xmin=0.01,
                 xmax=0.99,
                 ymin=0.01,
                 ymax=0.99,
                 label='box2',
                 colour="gray60",
                 fill="white",
                 gg=NULL,
                 addSheets=0,
                 size=4,
                 print=FALSE){
  
  #require(ggplot2)
  if(is.null(gg)) gg <- ggplot()
  #or for easier blank background plotting
  #require(cowplot) #cowplot on github
  #if(is.null(gg)) gg <- ggdraw()
  
  boxes <- data.frame(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    label = label
  )
  
  #add label positions (todo allow args)
  boxes$x <- boxes$xmin + 0.01
  boxes$y <- boxes$ymax - 0.04   
  
  #adding sheets (blank boxes behind)
  if (addSheets>0)
  {
    #copy coords
    sheets <- boxes
    for(sheetNum in addSheets:1)
    {
      #cat(sheet)
      sheets$xmin<-boxes$xmin+sheetNum*0.01
      sheets$xmax<-boxes$xmax+sheetNum*0.01
      sheets$ymin<-boxes$ymin-sheetNum*0.01
      sheets$ymax<-boxes$ymax-sheetNum*0.01
      gg <- gg + geom_rect(data=sheets, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),colour=colour, fill=fill)
    }
  }

  gg <- gg + geom_rect(data=boxes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), colour=colour, fill=fill)
  
  gg <- gg + geom_text(data=boxes, aes(x=x, y=y, label=label, hjust=0, vjust=0),size=size)
  
  if(print) print(gg)
  
  invisible(gg)
  
}




#ggsave("filename.pdf")





