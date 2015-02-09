\name{viewRaw}
\alias{viewRaw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{View the Raw Binary Content of a File}
\description{
  Displays the raw bytes of a file like a hex editor, showing
  offsets within the file, raw bytes in binary or hexadecimal form,
  and a human-readable representation of the bytes as either
  ASCII characters, integers, or real values.
}
\usage{
viewRaw(..., page = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Arguments passed to the function \code{readRaw}.}
  \item{page}{If \code{TRUE}, the output is sent to the application
  set up to display text files as per \code{getOption("pager")}.}
}
\details{
  This function is only called for its side-effect, which is
  to display the file.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{readRaw}}
  \code{\link{viewFormat}}
}
\examples{
viewRaw(hexViewFile("rawTest.txt"), width=8)
viewRaw(hexViewFile("rawTest.txt"), machine="binary", width=4)

# UNICODE text
# rawTest.unicode created using Notepad on Windows
viewRaw(hexViewFile("rawTest.unicode"), width=8)

viewRaw(hexViewFile("rawTest.int"), human="int")
viewRaw(hexViewFile("rawTest.real"), human="real", width=8, endian="big")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
\keyword{ connection }
