### extracting the content after a header of one email
##e.g.,  extvalfun(header) to extract the content after Tag

extvalfun = function(txt){
          con = textConnection(txt)
          valuematrix = read.dcf(con)
          close(con)
          return(valuematrix)
}