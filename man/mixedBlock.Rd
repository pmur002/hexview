\name{mixedBlock}
\alias{mixedBlock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a mixedBlock Object }
\description{
  This function creates a \code{"mixedBlock"} object, which is
  a description of a block of binary data.  This can be used
  as part of a description of a binary format.
}
\usage{
mixedBlock(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ One or more \code{"memBlock"} objects. }
}
\details{
  A \code{"mixedBlock"} object describes a block of binary data that consists
  of a series of sub-blocks.  Each sub-block can be any type
  of \code{"memBlock"} object.
}
\value{
  A \code{"mixedBlock"} object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{memFormat}}
  \code{\link{readFormat}}
  \code{\link{memBlock}}
  \code{\link{atomicBlock}}
  \code{\link{vectorBlock}}
  \code{\link{lengthBlock}}
  \code{\link{markedBlock}}
}
\examples{
# A line of text followed by a four-byte integer
mixedBlock(ASCIIline, integer4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
