\name{markedBlock}
\alias{markedBlock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a markedBlock Object }
\description{
  This function creates a \code{"markedBlock"} object, which is
  a description of a block of binary data.  This can be used
  as part of a description of a binary format.
}
\usage{
markedBlock(marker=integer4,
            switch=function(marker) { ASCIIchar },
            markerLabel="marker", blockLabel="block")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marker}{ A \code{"memBlock"} object. }
  \item{switch}{ A function that returns a \code{"memBlock"} object,
    or \code{NULL}. }
  \item{markerLabel}{ A label to name the marker component of the
    \code{"markedBlock"} object.}
  \item{blockLabel}{ A label to name the component that is read
    after the marker component of the
    \code{"markedBlock"} object.}
}
\details{
  A \code{"markedBlock"} object describes a block of binary data that consists
  of a \dQuote{marker} block containing information on further blocks.
  The \code{marker} block is read first, then this block is passed
  to the \code{switch} function.  The \code{switch} function
  can look at the contents of the \code{marker} block and decide
  what sort of block should be read next (the \code{blockValue()}
  function may be useful here).  The result of the
  \code{switch} function is read from the end of the \code{marker}
  block.  The \code{marker} block and the result of the \code{switch}
  function can be any type
  of \code{"memBlock"} object.  If the result of the \code{switch()}
  function is \code{NULL} then only the \code{marker} block is returned
  (no further memory is read).
}
\value{
  A \code{"markedBlock"} object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{memFormat}}
  \code{\link{readFormat}}
  \code{\link{memBlock}}
  \code{\link{atomicBlock}}
  \code{\link{vectorBlock}}
  \code{\link{lengthBlock}}
  \code{\link{mixedBlock}}
  \code{\link{blockValue}}
}
\examples{
# A single-byte integer which dictates how many
# subsequent four-byte reals to read
markedBlock(integer1,
            function(marker) {
              lengthBlock(real4, blockValue(marker))
            })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
