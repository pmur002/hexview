\name{vectorBlock}
\alias{vectorBlock}
\alias{lengthBlock}
\alias{ASCIIline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a vectorBlock Object }
\description{
  These functions create a \code{"vectorBlock"} or
  \code{lengthBlock} object, which are
  a descriptions of a block of binary data.  These can be used
  as part of a description of a binary format.
}
\usage{
vectorBlock(block = ASCIIchar, length = 1)
lengthBlock(length = integer4, block = ASCIIchar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{block}{ An object derived from the \code{"memBlock"}
    class, e.g., an \code{"atomicBlock"} object. }
  \item{length}{ The number of \code{block} objects in
    the overall binary block. }
}
\details{
  These objects describe a block of binary data that consists
  of a repeating sub-block.  The sub-block can be any type
  of \code{"memBlock"} object.

  There is also a predefined \code{ASCIIline} block, which is
  a block of single-byte characters terminated by a newline
  character.

}
\value{
  A \code{"vectorBlock"} or
  \code{lengthBlock} object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{memFormat}}
  \code{\link{readFormat}}
  \code{\link{memBlock}}
  \code{\link{atomicBlock}}
  \code{\link{mixedBlock}}
  \code{\link{markedBlock}}
}
\examples{
# A block of 20 four-byte blocks
# which are interpreted as integer values
vectorBlock(integer4, 20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
