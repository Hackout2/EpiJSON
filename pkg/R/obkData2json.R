#' output an \code{OutBreakTools::obkData} object to JSON
#' 
#' output an \code{obkData} object or part of one to a JSON string or file


#' @param x an obkData object or part of one
#' #@param file optional filename for JSON string
#' 
#' @return ?maybe a string of the JSON
#' @export

##################
## obkData2json ##
##################
obkData2json <- function(x){
    ## CHECK PRESENCE OF INDIVIDUAL INFORMATION ##
    if(){
        ## generate empty object ##
        warning("No individual information - returning empty object")
        return()
    }

    ## SHAPE DATA INTO LIST: ##
    ## $indiv1:
    ##   $id
    ##   $attributes
    ##     $name1 $type $value
    ##     $name2
    ##     $name3
    ##   $records
    ##     $record1
    ##       $id
    ##       $name
    ##       $date
    ##       $location
    ##       $attributes
    ##         $attribute1
    ##         $attribute2
    ##     $record2
    ##       $id
    ##       $name
    ##       $date
    ##       $location
    ##       $attributes
    ##         $attribute1
    ##         $attribute2
    ##         $attribute3


    ## AUXIL FUNCTION TO PROCESS ONE INDIVIDUAL ##
    ## x: obkData
    ## id: individual id
    getInfoOneIndiv <- function(x, id){
        ## get meta info on individual ##

        ## get list of records for this individual ##

        ## build output list ##

        ## return result ##
    }


    ## AUXIL FUNCTION TO GENERATE ATTRIBUTES FROM NAMED VALUES ##
    f1 <- function(x) toJSON(list(name=names(x), type=typeof(x), value=x))

    ## PROCESS INDIVIDUALS ##

    ## GET LIST TO BE CONVERTED ##

    ## CONVERT TO JSON ##
}
