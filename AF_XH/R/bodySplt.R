

# given list `headers' of header lines from discr(),
# obtain the indices of corresponding email bodies.
# Note that this will break if any emails have empty
# bodies! If any appear in the data, we should throw
# them out.

bodySplt <- function(linz, headers){
    numLins <- length(linz)
    numHds <- length(headers)
    # heds <- unlist(headers)


    notheads <- lapply(2:numHds, function(h, headers){
        
        m1 <- max( headers[[ h-1 ]] ) + 1
        m2 <- min( headers[[ h ]] ) - 1
        return(m1:m2)
        }, headers = headers)


    notheads <- c( notheads, 
        list(( max( headers[[ numHds ]] ) + 1 ):numLins  ))
        # linz[max(headers[[length(headers)]]),length(linz)]) # adds last bit
    return(notheads)
    }