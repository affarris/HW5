###remove unuseful information from list based on "z"
###eg in r.help data, only keeping the message replying questions

    Replysplit = function (list, x, y, z){
         if (x == y){
            RowInd = which(unlist(sapply(list, function(x) is.na(x[["z"]]))) == FALSE)
            for (i in 1 : length(RowInd)){
                 list[[i]] = list[[RowInd[i]]]
                 }
        } else {
            RowInd =  which(unlist(sapply(list, function(x) is.na(x[["z"]]))) == TRUE)
            for (i in 1 : length(RowInd)){
                  list[[i]] = list[[RowInd[i]]]
              }
        }
        return (list)
   }