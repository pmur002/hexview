\name{print.rawFormat}
\alias{print.rawFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Print Method for Binary File Format }
\description{
  This function displays a \code{"rawFormat"} object.
  Each line of output contains a binary offset, the binary data
  in a raw machine format, and an interpretation of
  the data in a human-readable format. The format
  consists of one or more sub-blocks and a heading line
  is added for each block.
  The object contains parameters controlling the format
  of the display, some of which may be overridden in
  the call to \code{print}.
}
\usage{
\method{print}{rawFormat}(x, sep1 = "  :  ", sep2 = "  |  ",
                  blockHead = TRUE, blockChar = "=", page = FALSE, ...)
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
  \item{page}{ If \code{TRUE} the output is sent to the
    file viewer specified by \code{getOption("pager")}.}
  \item{\dots}{ Other arguments to \code{print}. }
}
\details{
  Information on the number of bytes on each line, the machine representation
  of each byte and the human-readable format are all taken from the
  taken from the \code{"rawBlock"} elements of the \code{"rawFormat"}
  object.   Consequently each block can have a quite different
  appearance.  Considerable effort is made to line up the separators
  across all blocks within the format.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{readFormat}}
  \code{\link{as.character.rawFormat}}
}
\examples{
fileFormat <- readFormat(hexViewFile("rawTest.int"),
                         memFormat(int1=integer4, int2=integer4))
print(fileFormat)
print(fileFormat, sep2=":")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
