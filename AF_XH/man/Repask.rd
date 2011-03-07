\name{Repask}
\alias{Repask}
\title{
splitting the object
}
\description{
 split the object into two group based on if the content contain NA
}
\usage{
Repask = function (list, x, y, z)
}
\arguments{
  \item{list}{A list; each entry is a list of list.
}
  \item{x}{condition 1
}
  \item{y}{condition 2
}
  \item{z}{condition 3
}  
}
\value{
A  list;
}
\author{
XH_YC
}
\examples{
#tagvalue is a list of lists of headers.
Tag.reply = lapply(tagvalue, function(x) Repask(x, "reply", "reply", "In-Reply-To"))
}