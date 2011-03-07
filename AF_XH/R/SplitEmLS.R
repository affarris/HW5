###Split a list of lists of emails into 
###different parts (Probably headers and bodies

SplitEmLS=function(x,n){
        for (i in 1: length(x)){
              x[[i]]= sapply(x[[i]], function(y){y = y[[n]]})    
                              } 
             return(x)
   }