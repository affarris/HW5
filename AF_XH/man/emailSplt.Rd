\name{emailSplt}
\alias{emailSplt}
\title{
Obtain list of emails
}
\description{
Using known header and body indices, creates a list organizing the emails themselves.
}
\usage{
bodySplt(linz, headers, bodies)
}
\arguments{
  \item{linz}{A character vector, with lines from emails
}
  \item{headers}{A list; each entry corresponds to an email, and contains the indices of the lines corresponding to that email's header. Obtained from function discr .
}
  \item{bodies}{A list; each entry corresponds to an email, and contains the indices of the lines corresponding to that email's body. Obtained from function bodySplt .
}
}
\value{
A list, of length the number of emails; each entry is a list with two elements, one the character vector of email header, the other the character vector of email body.
}
\author{
AF
}
\examples{
 foo <- c("HEADER","","not a header", "not a header","", "also not a header", "HEADER", "", "still not a header")
HD <- discr(foo, "HEADER") 
BD <- bodySplt(foo, HD)
emailSplt(foo, HD, BD)
}