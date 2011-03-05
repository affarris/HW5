subHeaders <- function(emailHeader, fields, is.dcf) {
    if (is.dcf == TRUE){
            tCon <- textConnection(emailHeader)
            haed <- read.dcf(tCon, fields = fields )
            close(tCon)
            return( as.list( haed[ 1, fields ] ) )

    }
    else{
            fields <- paste( , )



    }


    }, fields = fields)


