### extracting the content after a header of one email
##e.g.,  extvalfun(header,"From") to extract the content after "From:" 

extvalfun = function(header, tag){
          con = textConnection(header)
          valuematrix = read.dcf(con)
          close(con)
          value = valuematrix[,tag]
          return(value)
      }
  
