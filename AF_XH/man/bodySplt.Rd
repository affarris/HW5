\name{bodySplt}
\alias{bodySplt}
\title{
Obtain row indices of bodies of emails
}
\description{
Using known headers, identifies lines correspinding to email bodies.
}
\usage{
bodySplt(linz, headers)
}
\arguments{
  \item{linz}{A character vector, with lines from emails
}
  \item{headers}{A list; each entry corresponds to an email, and contains the indices of the lines corresponding to that email's header. Obtained from function discr .
}
}
\value{
A list, of length the number of identified headers; each entry is an integer vector containing line indicies for the respective email body.
}
\author{
AF
}
\note{
This will break if any emails have empty bodies. If this is the case, such emails should be removed in pre-processing.
}
\examples{
 foo <- c("HEADER","","not a header", "not a header","", "also not a header", "HEADER", "", "still not a header")
HD <- discr(foo, "HEADER") 
bodySplt(foo, HD)
}