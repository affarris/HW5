
# takes lists of email header and body indices, and uses them to obtain
# a list of lists, each with the bodies and headers in them.

emailSplt <- function(linz, headers, bodies){
    numHds <- length(headers)

    emails <- lapply(1:numHds, function(j, linz, headers, bodies){
            list(header = linz[ headers[[ j ]] ],
                body = linz[ bodies[[ j ]] ] )
                }, linz = linz, 
                headers = headers,
                bodies = bodies)
    return(emails) # <--- function's output
    }