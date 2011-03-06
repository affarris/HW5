
# make graphNEL object, using edgelist:
# (from result of function longtofrom() )
# requires library(graph)


graphobj <- function( long ){
    nods <- unique( as.character( long ) )
    Edgs <- lapply( nods, function( one, long ){
        Wh <- long[ long[,1] == one, 2 ]
        tos <- rle( as.character( sort( as.factor( Wh ) ) ) )
        return( list( edges = tos$values, weights = tos$lengths ) ) 
        }, long = long )
    names( Edgs ) <- nods
    return(new("graphNEL", nodes = nods, edgeL = Edgs, edgemode = "directed"))
    }




