\name{readRaw}
\alias{readRaw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read the Raw Binary Content of a File}
\description{
  Read the contents of a file as bytes and create an object
  containing the raw data, plus
  optionally an interpretation of the bytes as numeric values,
  plus parameters controlling how to display the data.
}
\usage{
readRaw(file, width = NULL, offset = 0, nbytes = NULL,
        machine = "hex", human = "char",
        size = switch(human, char = 1, int = 4, real = 8),
        endian = .Platform$endian, signed = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The name of a file or a connection.}
  \item{width}{The number of bytes to print per row when displaying the data.}
  \item{offset}{An offset within the file to start reading.}
  \item{nbytes}{The number of bytes to read from the file.
    \code{NULL} means read the whole file.}
  \item{machine}{How to print each byte when displaying; either
    \code{"hex"} or \code{"binary"}.}
  \item{human}{How to print a human-readable form of the data;
    either \code{"char"} (an ASCII character),
    \code{"int"} (an integer), or \code{"real"} (a floating point number).}
  \item{size}{The number of bytes used to generate each value
    when interpreting the raw binary as character or numeric data.}
  \item{endian}{The endianness of the binary data;  used when
    interpreting bytes as numeric values.}
  \item{signed}{Whether the bytes should be interpreted as a
    signed numeric value.}
}
\details{
  Each individual byte is printed in the appropriate \code{machine}
  format, but there is only one value printed in the appropriate
  \code{human} format for every \code{size} bytes.  Consequently,
  the \code{width}
  must be a multiple of the \code{size}.  
}
\value{
  An object of class \code{"rawBlock"}.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{viewRaw}}
  \code{\link{readBin}}
  \code{\link{as.character.rawBlock}}
  \code{\link{print.rawBlock}}
  \code{\link{blockValue}}
  \code{\link{readFormat}}
}
\examples{
readLines(hexViewFile("rawTest.txt"))

fileBlock <- readRaw(hexViewFile("rawTest.txt"), width=8)
blockValue(fileBlock)

fileBlock <- readRaw(hexViewFile("rawTest.int"), human="int")
blockValue(fileBlock)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
\keyword{ connection }
