\name{readEViews}
\alias{readEViews}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Read an Eviews File }
\description{
  This function reads a file in Eviews format (Eviews is
  an econometrics package).
}
\usage{
readEViews(filename, time.stamp=TRUE, as.data.frame = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ The name of the file. }
  \item{time.stamp}{ A logical indicating whether to include a
    \code{Date} column in the result. }
  \item{as.data.frame}{ If \code{TRUE} the result is a data frame;
    otherwise a list of variables is returned. }
}
\details{
  This function is just a demonstration of how the functions in
  this package can be used to read a complex binary format.  It has
  been tested on a few sample files (and works), but there is no
  guarantee it will work for all Eviews files (this is not helped
  by the fact that it is based on reverse-engineering information
  about the Eviews format, NOT an official description of the format.
}
\value{
  Either a data frame or a list of variables.
}
\references{ \url{http://www.eviews.com/} and
  \url{http://www.ecn.wfu.edu/~cottrell/eviews_format/}}
\author{ Paul Murrell }
\examples{
readEViews(hexViewFile("data4-1.wf1"))
}
\keyword{ file }
