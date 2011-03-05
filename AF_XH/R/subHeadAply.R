subHeadAply <- function( emailHeader, fields, is.dcf ){
    if ( length(fields) > 1 ){
         L <- lapply(fields, function( afield, emailHeader, is.dcf ){
            sapply(emailHeader, function( eH, afield, is.dcf ){
                subHeaders( eH$header , afield, is.dcf )
                }, afield = afield, is.dcf = is.dcf
            )
            }, emailHeader = emailHeader, is.dcf = is.dcf
        )
        names(L) <- fields
        return(L)
        }
    else{
        sapply(emailHeader, function( eH, fields, is.dcf ){
            subHeaders( eH$header , fields, is.dcf )
            }, fields = fields, is.dcf = is.dcf
            )
    }
}