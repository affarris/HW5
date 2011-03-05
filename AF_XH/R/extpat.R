 
### Extract the interested part of the header ####

extpat=function(hdlist, reexp, el="\\1"){
    for (i in 1:length(hdlist)){
        for (j in 1: length(hdlist[[i]])){
            tt=hdlist[[i]][j]
            hdlist[[i]][j]= gsub(reexp,el,tt ) 
                                }
                    } 
     return(hdlist)
      }
