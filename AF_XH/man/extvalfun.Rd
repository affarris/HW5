\name{extpat}
\alias{extpat}
\title{
 extract a specific pattern from a list 
}
\description{
 Using gsub() to extract a sepcific pattern(regular expression) from a list
}
\usage{
extpat=function(hdlist, reexp, el="\\1")
}
\arguments{
  \item{hdlist}{A list; each entry is a vector of a specific kind header of a email list.
}
  \item{reexp}{regular expression to identify a certain pattern.
}
  \item{el}{the element which is used as replacement in gsub().
}
}
\value{
A a list; each entry is the interested pattern of a header
}
\author{
XH
}
\examples{
foo <- as.list(c("From: poliquin at sas.upenn.edu (Chris Poliquin)","From: cgb at datanalytics.com (Carlos J. Gil Bellosta)")
reexp=".*\\((.*)\\)$"
extpat(foo, reexp)
}