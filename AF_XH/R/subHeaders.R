subHeaders <- function(emailHeader, fields, is.dcf) {
    if (is.dcf == TRUE){
            tCon <- textConnection(emailHeader)
            head <- read.dcf(tCon, fields = fields )
            close(tCon)
            return( as.list( head[ 1, fields ] ) )

    }
    else{    
           fields1 <- sapply(fields, function(w){ paste("(","^",w,":",")", sep = "") })  
           fields2 <- paste(fields, collapse = "|")

            rInd = grep(fields2,  emailHeader)
               tt= gsub(".*\\:[= ](.*)$","\\1", hd[rInd])
            names(tt)=fields
            return( as.list( head[ 1, fields ] ) )
    }
    }, fields = fields)


