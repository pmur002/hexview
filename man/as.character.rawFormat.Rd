\name{as.character.rawFormat}
\alias{as.character.rawFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert Binary File Format into Strings }
\description{
  This function takes a \code{"rawFormat"} object and
  generates human-readable strings for displaying the format.
  Each string contains a binary offset, the binary data
  in a raw machine format, and an interpretation of
  the data in a human-readable format.  The format
  consists of one or more sub-blocks and a heading line
  is added for each block.
}
\usage{
\method{as.character}{rawFormat}(x, sep1 = "  :  ", sep2 = "  |  ",
                blockHead = TRUE, blockChar = "=", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A \code{"rawFormat"} object. }
  \item{sep1}{ A separator to insert between the format offset
    and the machine format.}
  \item{sep2}{ A separator to insert between the machine format
    and the human-readable format.}
  \item{blockHead}{ A logical indicating whether to print
    a header between blocks of the format. }
  \item{blockChar}{ The character used as a prefix to
    the block names for printing headers between blocks. }
  \item{\dots}{ Other arguments to \code{as.character}. }
}
\details{
  Information on the number of bytes on each line, the machine representation
  of each byte and the human-readable format are all taken from the
  taken from the \code{"rawBlock"} elements of the \code{"rawFormat"}
  object.   Consequently each block can have a quite different
  appearance.  Considerable effort is made to line up the separators
  across all blocks within the format.
}
\value{
  A character vector.
}
\author{ Paul Murrell }
\seealso{ 
  \code{\link{readFormat}}
  \code{\link{print.rawFormat}}
}
\examples{
fileFormat <- readFormat(hexViewFile("rawTest.int"),
                         memFormat(int1=integer4, int2=integer4))
as.character(fileFormat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
