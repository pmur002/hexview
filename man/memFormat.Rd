\name{memFormat}
\alias{memFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a memFormat Object }
\description{
  This function creates a \code{"memFormat"} object which is
  a description of a binary file format.  
}
\usage{
memFormat(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ One or more \code{"memBlock"} objects. }
}
\details{
  A \code{"memFormat"} object is made up of one or more
  \code{"memBlock"} objects.
}
\value{
  A \code{"memFormat"} object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{readFormat}}
  \code{\link{memBlock}}
  \code{\link{atomicBlock}}
  \code{\link{vectorBlock}}
  \code{\link{lengthBlock}}
  \code{\link{mixedBlock}}
  \code{\link{markedBlock}}
}
\examples{
memFormat(int1=integer4, int2=integer4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
