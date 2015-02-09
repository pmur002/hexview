\name{blockValue}
\alias{blockValue}
\alias{blockString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract the Value of a Binary Block }
\description{
  The \code{blockValue} function returns the interpreted value of a block
  of binary data (a \code{"rawBlock"} object).

  The \code{blockString} function returns a null-terminated string
  from a block
  of binary data that is interpreted as a character data.
}
\usage{
blockValue(block)

blockString(block)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{block}{ A \code{"rawBlock"} object. }
}
\details{
  The type
  of the value returned is determined when the binary
  block is created (e.g., by \code{readRaw})
  \emph{not} when the value is extracted
  by the \code{blockValue} function.

  The \code{blockString} function is useful for extracting
  a value from a binary block which is a string padded with
  null characters.

  The \code{blockString} function throws an error if the
  block is not interpreted as a character value.
}
\value{
  Either a character, or numeric vector depending on how the
  binary block should be interpreted.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{readRaw}}
  \code{\link{readFormat}}
}
\examples{
charBlock <- readRaw(hexViewFile("rawTest.txt"), width=8)
blockValue(charBlock)
blockString(charBlock)

intBlock <- readRaw(hexViewFile("rawTest.int"), human="int")
blockValue(intBlock)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
