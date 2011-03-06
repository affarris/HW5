times <- function( emails, format, is.dcf ){
        timelines <- subHeadAply( emails , "Date", is.dcf )
        strptime( timelines, format )
        }

# note that there can be only one date for each email, so that
# subHeadAply results in a vector (due to use of sapply in that
# function).

# value is an object of class "POSIXlt".
# for Enron data, should have format = "%a, %d %b %Y %H:%M:%S %z"