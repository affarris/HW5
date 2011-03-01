# code for reading in a single .txt.gz R-help file
# since a new email always begin with two lines "From" and "From:"
## got the indices of rows in each case and then check if "From" and "From:" are two concecutive lines.
## if so, it is the beginning of a new email.





readData=function(file){

   con=file(file, "r")
   txt=readLines(con)

   rows1=grep("^From ", txt)
   rows2=grep("^From: ", txt)  
   mat=sapply(rows1, function(x){(x+1) %in% rows2})
   begrs=rows1[which(mat)]

   numMess=length(begrs)
   endrs=c((begrs[2:numMess]-1),length(txt))
   introws=as.matrix(cbind(begrs, endrs))
   messagelist = apply(introws, 1, function(x) {txt[(x[1]:x[2])] })
   list(numberofemail=numMess, emails=messagelist)                  

}

