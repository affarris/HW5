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



# B Part: 
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

#split the header and body for enron email, and save each header and body of each email into a list
txtspliteron =  subsetfun(txteron, "^Date\\:")
headereron =  txtspliteron[[1]]
 
txtsplitrhelp =  lapply(txtrhelp, function(x) subsetfun(x,"(^From) (.*) ([0-9]{4}$)"))
headerrhelp = sapply(txtsplitrhelp, function(x) x = x[[1]])
bodyrhelp = sapply(txtsplitrhelp, function(x) x = x[[2]])

######################################
####extract value from the dcf format################
extvalfun = function(txt){
          con = textConnection(txt)
          valuematrix = read.dcf(con)
          close(con)
          return(valuematrix)
}


#########extract value of from, to, and so on#############
tagvalue =sapply(headerrhelp, function(x) sapply(x, function(y) extvalfun(y)))
rhelpextract = function(list, tag){
         value = lapply(list, function(x) as.matrix(unlist(lapply(x, function(y) y = y[,tag]))))
         return(value)
}
 

######tesr 

From.v1 = rhelpextract(tagvalue, "From")
Date.v = rhelpextract(tagvalue, "Date")
Subject.v = rhelpextract(tagvalue, "Subject")



#################################################
### split the tag list "tagvalue" into two part base on  "In-Reply-To"
   Replysplit = function (list, x, y, z){
         RowInd = which(unlist(sapply(list, function(x) sum(colnames(x) == z))) == 1)
         if (x == y){  
            aa = sapply(RowInd, function(x) x = list[[x]])
        } else {
            RowInd = c(1: length(list))[-RowInd]
            aa = sapply(RowInd, function(x) x = list[[x]])
            }
        return (aa)
   }
Tag.reply = sapply(tagvalue, function(x) Replysplit(x, "reply", "reply", "In-Reply-To"))
Tag.ask =  sapply(tagvalue, function(x) Replysplit(x, "ask", "reply", "In-Reply-To"))
Reply.v.reply =  rhelpextract(Tag.reply, "From")
Reply.v.ask =  rhelpextract(Tag.ask, "From")
###################################################################


################################################### 
