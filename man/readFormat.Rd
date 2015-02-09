\name{readFormat}
\alias{readFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Read a Binary File }
\description{
  Read the raw binary content of a file using a description
  of the binary format.
}
\usage{
readFormat(file, format, width = NULL, offset = 0, machine = "hex",
           flatten = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The name of a file or a connection.}
  \item{format}{ A \code{"memFormat"} object. }
  \item{width}{The number of bytes to print per row when displaying the
    file. }
  \item{offset}{An offset within the file to start reading.}
  \item{machine}{How to print each byte when displaying the file; either
    \code{"hex"} or \code{"binary"}.}
  \item{flatten}{If \code{TRUE} the list of blocks created from the
    \code{"memFormat"} description are flattened to a list of depth 1.}
}
\details{
  This function uses a \code{"memFormat"} description to
  read the raw binary content of a file and interpret
  sub-blocks of the file as distinct (blocks of) values.

  The \code{"memFormat"} can described a nested structure of
  blocks.  The \code{flatten} argument is used to convert
  nested format structures to a flat (depth of one) structure.

  The format is always flattened for display, but extracting
  
}
\value{
  A \code{"rawFormat"} object, which is a list:
  \item{blocks}{A list (of lists) of \code{"rawBlock"} objects. }
  \item{offset}{The offset in the file where reading began.}
  \item{nbytes}{The number of bytes read from the file.}
}
\author{ Paul Murrell }
\seealso{
  \code{\link{viewFormat}}
  \code{\link{memFormat}}
  \code{\link{as.character.rawFormat}}
  \code{\link{print.rawFormat}}
  \code{\link{readRaw}}
  \code{\link{readBin}}
}
\examples{
fileFormat <- readFormat(hexViewFile("rawTest.int"),
                         memFormat(int1=integer4, int2=integer4))
blockValue(fileFormat$blocks$int2)

fileFormat <- readFormat(hexViewFile("rawTest.int"),
                         memFormat(integers=vectorBlock(integer4, 20)))
blockValue(fileFormat$blocks$integers)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
\keyword{ connection }
