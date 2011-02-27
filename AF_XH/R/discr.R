
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

discr <- function(linz, 
        heads = c("Date:","From:","To:","Subject:"),
        sensitivity = 0.75 ){
    blanks <- which(linz == "")
    headZ <- sapply(heads, function(w){ paste("(","^",w,")", sep = "") })
    headZ <- paste(headZ, collapse = "|")
    numHds <- length(heads)

    # yeZ <- grep(headZ, thing)
    yeZ <-  grepl(headZ, linz) 
    
    headSeekr <- function(u, yeZ, numHds, sensitivity){
        Yz <- yeZ[ u[2]:u[1] ]
        scr <- sum(Yz)
        if (scr / numHds >= sensitivity) ( min( which(Yz) ) + u[2] - 1):( u[1] )
        else NULL
        }
    
    blanks <- c(0, blanks)
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


