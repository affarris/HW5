OneHd= function(list, tag){
         value = lapply(list, function(x)  as.matrix(unlist(sapply(x, function(y)  y = y[[tag]]))))
               return(value)
                           } 