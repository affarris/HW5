

# given a list of emails, extracts addresses corresponding to 
# emails between speficified domains
# e.g. get addresses with messages from @enron.com to @scumbagfraudsters.com

tofrom <- function( emails, fromDomain, toDomain, is.dcf){
    addresses <- subHeadAply( emails, c( "From",  "To" ), is.dcf )
    fromDomain <- paste( fromDomain, collapse = "|" )
    toDomain <- paste( toDomain, collapse = "|" )
    whichFroms <- grepl( fromDomain, addresses$From )
    whichTos <- grepl( toDomain, addresses$To)
    boTH <- whichFroms & whichTos
    lapply( addresses , function( L, ind ){ L[ ind ] }, ind = boTH  )
    }
