extvalfun = function(txt){
          con = textConnection(txt)
          valuematrix = read.dcf(con)
          close(con)
          return(valuematrix)
}