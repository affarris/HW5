\name{SplitEmLS}
\alias{SplitEmLS}
\title{
 Split a list of lists of emails
}
\description{
 Split a list of lists of emails into different parts, for example, headers and bodies
}
\usage{
  Split=function(x,n)
}
\arguments{
  \item{x}{A list; each entry is a list of lists of emails.
}
  \item{n}{the index of elememt in an email.
}
  
}
\value{
A  list; each entry is a list of part of emails
}
\author{
XH_YC
}
\examples{
## wholelist is a list of lists of emails, 1 is the index of the header part in an email
 header.R= Split(wholelist, 1)
## header.R is a list of lists of email headers

}
