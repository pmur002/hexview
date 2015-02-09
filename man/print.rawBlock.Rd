\name{print.rawBlock}
\alias{print.rawBlock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Print Method for Block of Binary Data }
\description{
  This function displays a \code{"rawBlock"} object.
  Each line of output contains a binary offset, the binary data
  in a raw machine format, and an interpretation of
  the data in a human-readable format.
  The object contains parameters controlling the format
  of the display, some of which may be overridden in
  the call to \code{print}.
}
\usage{
\method{print}{rawBlock}(x, width = NULL, machine = NULL,
      sep1 = "  :  ", sep2 = "  |  ",
      showOffset = TRUE, showHuman = TRUE, 
      page = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A \code{"rawBlock"} object. }
  \item{width}{ The number of bytes to display per line of output. }
  \item{machine}{ The machine format to display;
    either \code{"hex"} or \code{"binary"}.}
  \item{sep1}{ A separator to insert between the block offset
    and the machine format.}
  \item{sep2}{ A separator to insert between the machine format
    and the human-readable format.}
  \item{showOffset}{ If \code{FALSE}, the column of offsets is
    not printed.}
  \item{showHuman}{ If \code{FALSE}, the column of human-readable format is
    not printed.}
  \item{page}{ If \code{TRUE} the output is sent to the
  file viewer specified by \code{getOption("pager")}.}
  \item{\dots}{ Other arguments to \code{print}. }
}
\details{
  If either \code{width} or \code{machine} is \code{NULL},
  the relevant value is taken from the
  \code{"rawBlock"} object.  The human-readable format is
  taken from the \code{"rawBlock"} object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{readRaw}}
  \code{\link{as.character.rawBlock}}
}
\examples{
fileBlock <- readRaw(hexViewFile("rawTest.txt"))
print(fileBlock)
print(fileBlock, width=8)
print(fileBlock, machine="binary", width=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
