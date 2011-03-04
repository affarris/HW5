## obtain a list of lists of a interested header line ####
 
greHD=function(char){
   
   hdlist=rep(list(list()),numFile)

   for (i in 1: numFile){
      
        hdm=matrix(0,numEmail[i])
        for (j in 1:numEmail[i]){
                  
                tt=wholelist[[i]][[3]][[j]]$header
                r= grep(paste("^",char,":",sep=""), tt)
                tt=tt[r]                
                hdm[j]=extvalfun(tt,char)    
                                    
                                }              
        hdlist[[i]]=hdm
                      }
      return(hdlist)
 }


emaillist = lapply (1: length(wholelist), function(i) wholelist[[i]][[3]])
headerrhelp = lapply(emaillist, function(x) sapply(x, function(y) y$header))
bodyrhelp =  lapply(emaillist, function(x) sapply(x, function(y) y$body))