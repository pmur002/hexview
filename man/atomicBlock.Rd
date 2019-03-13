\name{atomicBlock}
\alias{atomicBlock}
\alias{ASCIIchar}
\alias{integer1}
\alias{integer2}
\alias{integer3}
\alias{integer4}
\alias{integer8}
\alias{real4}
\alias{real8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create an atomicBlock Object }
\description{
  This function creates an \code{"atomicBlock"} object, which is
  a description of a block of binary data.  This can be used
  as part of a description of a binary format.
}
\usage{
atomicBlock(type = "char", width = NULL, machine = "hex",
            size = switch(type, char = 1, int = 4, real = 8),
            endian = "little", signed = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{ How the block of binary data will be interpreted.
    either \code{"char"} (an ASCII character),
    \code{"int"} (an integer), or \code{"real"} (a floating point number).}
  \item{width}{The number of bytes to print per row when displaying the block.}
  \item{machine}{How to print each byte when displaying; either
    \code{"hex"} or \code{"binary"}.}
  \item{size}{The number of bytes used to generate each value
    when interpreting the raw binary as character or numeric data.}
  \item{endian}{The endianness of the binary data;  used when
    interpreting bytes as numeric values.}
  \item{signed}{Whether the bytes should be interpreted as a
    signed numeric value.}
}
\details{
  An \code{"atomicBlock"} object describes a binary block
  representing a single value.

  Several standard binary types are predefined (with common
  C type equivalents in brackets):
  \code{ASCIIchar} (char),
  \code{integer1} (signed char),
  \code{integer2} (short),
  \code{integer3},
  \code{integer4} (int, long),
  \code{integer8} (long long),
  \code{real4} (float),
  \code{real8} (double).
}
\value{
  An \code{"atomicBlock"} object.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{memFormat}}
  \code{\link{readFormat}}
  \code{\link{memBlock}}
  \code{\link{vectorBlock}}
  \code{\link{lengthBlock}}
  \code{\link{mixedBlock}}
  \code{\link{markedBlock}}
}
\examples{
# A C long
atomicBlock("int", size=4)
integer4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
