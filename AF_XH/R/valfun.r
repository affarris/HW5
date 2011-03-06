rhelpextract = function(list, tag){
         value = lapply(list, function(x) as.matrix(unlist(lapply(x, function(y) y = y[1,tag]))))
         return(value)
}
