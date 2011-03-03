
# grep for listed emails:
egrep <- function(char, part = "body",  L = em){
            if (part == "body")
            is <- sapply(L, function(l){any( grepl( char, l$body) ) })
            if (part == "header")
            is <- sapply(L, function(l){any( grepl( char, l$header) ) })
            which(is)
            } 


numSrch <- function(x, L = bd){
            is <- sapply(L, function(l){x %in% l})
            which(is)
            }
