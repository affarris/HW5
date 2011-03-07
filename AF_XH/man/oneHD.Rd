\name{OneHd}
\alias{OneHd}
\title{
 extract a certain header from a list of lists of headers
}
\description{
 grab one header out to obtain a list of vectors of a certain header
}
\usage{
OneHd= function(list, tag)
}
\arguments{
  \item{list}{A list; each entry is a list of lists of email headers.
}
  \item{tag}{the header name.
}
  
}
\value{
A  list; each entry is a vector of specific header
}
\author{
XH_YC
}
\examples{
#tagvalue is a list of lists of headers.
From.v = OneHd(tagvalue, "From")
}