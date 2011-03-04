 
### Extract the interested part of the header ####

extpat=function(list, reexp, el="\\1"){
    for (i in 1:numFile){
        for (j in 1: numEmail[i]){
            tt=list[[i]][j]
            list[[i]][j]= gsub(reexp,el,tt ) 
                                }
                    } 
     return(list)
      }