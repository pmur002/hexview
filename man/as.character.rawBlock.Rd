\name{as.character.rawBlock}
\alias{as.character.rawBlock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert Block of Binary Data into Strings }
\description{
  This function takes a \code{"rawBlock"} object and
  generates human-readable strings for displaying the block.
  Each string contains a binary offset, the binary data
  in a raw machine format, and an interpretation of
  the data in a human-readable format.
}
\usage{
\method{as.character}{rawBlock}(x, width = NULL, machine = NULL,
             sep1 = "  :  ", sep2 = "  |  ",
             showOffset = TRUE, showHuman = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A \code{"rawBlock"} object. }
  \item{width}{ The number of bytes to display per string. }
  \item{machine}{ The machine format to display;
    either \code{"hex"} or \code{"binary"}.}
  \item{sep1}{ A separator to insert between the block offset
    and the machine format.}
  \item{sep2}{ A separator to insert between the machine format
    and the human-readable format.}
  \item{showOffset}{ If \code{FALSE}, the column of offsets is
    not included in the strings.}
  \item{showHuman}{ If \code{FALSE}, the column of human-readable format is
    not included in the strings.}
  \item{\dots}{ Other arguments to \code{as.character}. }
}
\details{
  If either \code{width} or \code{machine} is \code{NULL},
  the relevant value is taken from the
  \code{"rawBlock"} object.  The human-readable format is
  taken from the \code{"rawBlock"} object.
}
\value{
  A character vector.
}
\author{ Paul Murrell }
\seealso{ 
  \code{\link{readRaw}}
  \code{\link{print.rawBlock}}
}
\examples{
fileBlock <- readRaw(hexViewFile("rawTest.txt"), width=8)
as.character(fileBlock)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
