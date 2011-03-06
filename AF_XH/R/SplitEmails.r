# the header start with the common title, such as Date:, From: and end up with white space
# split the header and body of the dataset based on the characterist of header
subsetfun = function (txt, pattern){
   #Find out indices of the beginning of hearder, the white spaces, and 
   #the white spaces bettween header and body.
    z1 = grep(pattern, txt)
    z2 = which (unlist(lapply(txt, nchar)) == 0)
    z3 = (sapply(1 : length(z1), function(i) 
                                         if (i < length(z1))
                                         z2[min(which(z2 < z1[i+1] & z2 > z1[i]))]
                                         else z2[min(which(z2 > z1[i]))]))
   # split emails based on Indices                                      
    subsetheader = sapply(1: length(z1), function(i) txt[z1[i] : (z3[i]-1)])
    subsetbody = sapply(1: length(z1), function(i)
                                        if (i < length(z1)) txt[z3[i] : (z1[i+1]-1)]
                                        else txt[z3[i] : length(txt)])
    return(list(subsetheader,subsetbody))
}