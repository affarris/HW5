\name{discr}
\alias{discr}
\title{
Algorithmically identify headers in emails.
}
\description{
This function allows a (in some ways) flexible and robust means of identifying headers in emails. 
}
\usage{
discr(linz, heads, sunsitivity)
}
\arguiments{
  \item{linz}{A character vector, with lines from emails
}
  \item{heads}{A character vector, with possible beginning header fields (E.g. "Date:","Subject:"); these will be matched only when they begin their respective line. Partial matching is not presently supported.
}
  \item{sensitivity}{The proportion of header fields present required to identify a `chunk' of lines as a header. For example (& by default), at least 3 out of 4 might be good. 
}

}
\value{
A list, of length the number of identified headers; each entry is an integer vector containing line indicies for the respective identified header.
}
\author{
AF
}
\note{
It is required that a blank line separate each header from the respective body of the email.

Also, something could go wrong if, for example, one header begins with "\tDate:" or "Dtae:"; if not enough header fields are specified and sensitivity is too low, the header may then be missed entirely. Even if not, the first line of the header would be included in the previous email's body! 

Also, WARNING: first header seems to contain `0 line' right now, will get this fixed soon.
}
\examples{
 foo <- c("HEADER","","not a header", "not a header","", "also not a header", "HEADER", "", "still not a header")

discr(foo, "HEADER") 

}