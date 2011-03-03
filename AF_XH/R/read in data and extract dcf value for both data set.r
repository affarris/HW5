#read in eronmail
con = gzfile("enron_mail_030204.tar.gz" )
txteron = readLines(con)
txtsub = txteron[2:204]

# read in rhelpe data set
datareadin = function (filename){
   filestring = list.files()
   t = length(filestring)
     txtrhelp = sapply(filestring, function(x){
                   con = file(x)
                   readLines(con)})
      return(txtrhelp)             
 }



# the header start with the common title, such as Date:, From: and end up with white space
# split the header and body of the dataset based on the characterist of header
subsetfun = function (txt, pattern){
    z1 = grep(pattern, txt)
    z2 = which (unlist(lapply(txt, nchar)) == 0)
    z3 = (sapply(1 : length(z1), function(i) 
                                         if (i < length(z1))
                                         z2[min(which(z2 < z1[i+1] & z2 > z1[i]))]
                                         else z2[min(which(z2 > z1[i]))]))
    subsetheader = sapply(1: length(z1), function(i) txt[z1[i] : (z3[i]-1)])
    subsetbody = sapply(1: length(z1), function(i)
                                        if (i < length(z1)) txt[z3[i] : (z1[i+1]-1)]
                                        else txt[z3[i] : length(txt)])
    return(list(subsetheader,subsetbody))
}

#split the header and body for enron & rhelp email, and save each header and body of each email into a list
txtspliteron =  subsetfun(txteron, "^Date\\:")
headereron =  txtspliteron[[1]]
 
txtsplitrhelp =  lapply(txtrhelp, function(x) subsetfun(x,"^From\\:"))
headerrhelp = sapply(txtsplitrhelp, function(x) x = x[[1]])


######################################
####extract value from the dcf format################
extvalfun = function(txt, tag){
          con = textConnection(txt)
          valuematrix = read.dcf(con)
          close(con)
          value = valuematrix[,tag]
          return(value)
}


#########extract value of from, to, and so on#############

eronextract = function(txt, tag){
          valueeron =  sapply(txt, function(x) extvalfun(x,tag))
}

rhelpextran = function(txt, tag){
          valuerhelp = sapply(txt, function(x) unlist(sapply(x, function(y) extvalfun(y, "From"))))
}

######tesr for to
tovalue = eronextract(headereron, "To")


 

 
