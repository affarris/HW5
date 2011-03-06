


# post-pre-processing:
# deals with some lines giving read.dcf() trouble

isnt.well.formed <- function(l){grep( "^[^:]+$" , l ) }


frontPend <- function(vec, ind, add = "\t"){
        vec[ ind ] = paste(add, vec[ ind ], sep = "")
        return(vec)
        }

inLine <- function(emails){
        lapply(emails, function(L, emails = emails){
                    bad <- isnt.well.formed(L$header)
                    if ( length(bad) > 0) L$header <- frontPend(L$header, bad)
                    return(L)
                    }, emails = emails
        )}

