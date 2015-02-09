\name{memBlock}
\alias{memBlock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a memBlock Object }
\description{
  This function creates a \code{"memBlock"} object, which is
  a description of a block of binary data.  This can be used
  as part of a description of a binary format.
}
\usage{
memBlock(nbytes = 1, width = NULL, machine = "hex")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nbytes}{ The number of bytes in the block. }
  \item{width}{The number of bytes to print per row when displaying the block.}
  \item{machine}{How to print each byte when displaying; either
    \code{"hex"} or \code{"binary"}.}
}
\details{
  A binary block that is read in from a file using this description
  is interpreted as \code{nbytes} single-byte characters.
  
  The description includes parameters controlling how a block of data
  should be displayed if this description is used to read in a block
  of binary data.
}
\value{
  A  \code{"memBlock"} object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{memFormat}}
  \code{\link{readFormat}}
  \code{\link{atomicBlock}}
  \code{\link{vectorBlock}}
  \code{\link{lengthBlock}}
  \code{\link{mixedBlock}}
  \code{\link{markedBlock}}
}
\examples{
  memBlock(8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
