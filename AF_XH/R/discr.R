
# this function identifies headers in a character vector containing 
# mutiple emails
# it needs a space at the end of each header, before the respective
# body of the email

# note that this method will be sensitive to small errors- if there 
# is some typo in 
# the first few characters of the first line of the header, or if
# not all header fields (e.g. "Date:") are specified, then, for example,
# the first line of a header might be missed and accidentally included in
# the previous email body.
# - perhaps should include some diagnostic tools for this case

# Summary:
# split up lines by those between pairs of consecutive blanks lines. 
# In each chunk, check how many relevant header line openings
# (e.g. "Date:") there are; if close enough (ie by sensitivity), then this 
# chunk contains a header, which begins at the earliest instance of 
# one of the line openings, and ends at the next blank line.
# This sacrifices efficiency for robustness
# (but it is only robust to some kinds of errors/ typos!)


# note: may break if sensitivity = 0!

# Note that the new argument/default for this function makes uses of
# it in its older form still valid.

discr <- function(linz, 
        heads = c("Date:","From:","Subject:","Mime-Version:"),
        sensitivity = 1, order = FALSE ){
    if (!order){
    blanks <- which(linz == "")
    headZ <- sapply(heads, function(w){ paste("(","^",w,")", sep = "") })
    headZ <- paste(headZ, collapse = "|")
    numHds <- length(heads)

    # yeZ <- grep(headZ, thing)
    yeZ <-  grepl(headZ, linz) 
    
    headSeekr <- function(u, yeZ, numHds, sensitivity){
        Yz <- yeZ[ u[2]:(u[1]-1) ]   ## We don't need to check the line which we already know is ""
        scr <- sum(Yz)
                    # this way, it won't be too greedy, and take too much for header:
        strth <- ceiling(sensitivity * numHds) 
        if (scr >= strth ) return (( u[2] + which(Yz)[scr - strth + 1] - 1  ):( u[1]-1 ) )
        else return(NULL)
        }
    
    if (blanks[1] != 1) blanks <- c(1, blanks)
    numBlks <- length(blanks)
    
    res <- sapply(as.data.frame(rbind(blanks[ 2:numBlks ], 
            blanks[ 1:(numBlks - 1) ])),
            headSeekr, 
            yeZ = yeZ, numHds = numHds, sensitivity = sensitivity )


    #res <- do.call(headSeekr, 
    #    list( u = blanks[-1], v = blanks[-numBlks],
    #        yeZ = yeZ, numHds = numHds, sensitivity = sensitivity
    #        ))    

    res[!sapply(res,is.null)]
    }

    else{ # (if order matters!):

    numHds <- length(heads)
    headZ <- sapply(heads, function(w){ paste("^",w, sep = "") })
    hdsLocs <- sapply(headZ, function(z, linz = linz){grepl(z, linz )}, linz = linz)
    
    # this will give us a matrix of whether each field is found in each line
    hdsLocs <- cbind(hdsLocs, linz == "" )
    rets <- apply(hdsLocs,1,any)
    # this stores for us which lines have matches on them (might be faster to do this first?))

    witch <- apply(hdsLocs[rets,],1,which)
    # this tells is which field is on each line

    diffrs <- as.character( c(0, diff(witch) ) )
    diffrs[ (hdsLocs[,1])[rets] ] <- "s" # we need to `anchor' our headers
    k <- paste( "s1{",numHds,"}", sep = "", collapse = "" )
    diffrs <- gsub("-[1:9]", "N", diffrs ) # negative sign will mess up indices!
    hedsTemp <- gregexpr(k, paste( diffrs , collapse = "" ) )[[1]] 
    # this tells us which of the candidate lines begin the headers.

    wrets <- which(rets) 

    # we still have to convert to origional indices:
    heds <- lapply(hedsTemp, function(h, wrets = wrets, numHds = numHds){
        L1 <- wrets[h]
        L2 <- wrets[h + numHds] - 1
        return(L1:L2)
        }, wrets = wrets, numHds = numHds)
    return(heds)
    }
}


