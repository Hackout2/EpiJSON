#' to view the structure of epijson objects and/or schema

#' testing ability/utility
#' structure is largely hardcoded for now, could try to automate later

#' @return a ggplot object
#' @export
epijsonObjectVis <- function(){

  
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





