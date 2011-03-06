

# this takes a list of to and from email addresses (from tofrom() )
# and puts them in a long form.
longtofrom <- function( tofroms, fromDomain, toDomain ){
    TOOs <- lapply(tofroms$To, function(L){strsplit(L, ", *" )})
    inds <- sapply( TOOs, function( z ){ length( unlist(z) ) } )
    Tos <- unlist(TOOs)
    Froms <- ( tofroms$From )[rep( 1:length(tofroms$From), inds)]
    Tos <- gsub( "\n", "", Tos )
    Froms <- gsub( "\n", "", Froms )
    return(cbind(Tos,Froms))
    }