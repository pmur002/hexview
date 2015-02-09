\name{viewFormat}
\alias{viewFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ View a Binary File }
\description{
  Displays the raw bytes of a file like a hex editor, showing
  offsets within the file, raw bytes in binary or hexadecimal form,
  and a human-readable representation of the bytes as either
  ASCII characters, integers, or real values.  The file
  is broken up into blocks according to a supplied file format
  specification.
}
\usage{
viewFormat(..., page = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ Arguments passed to the function \code{readFormat},
    most importantly, a \code{"memFormat"} describing the file format.}
  \item{page}{If \code{TRUE}, the output is sent to the application
    set up to display text files as per \code{getOption("pager")}.}
}
\details{
  This function is only called for its side-effect, which is
  to display the file.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{readFormat}}
  \code{\link{viewRaw}}
}
\examples{
viewFormat(hexViewFile("rawTest.int"),
           memFormat(int1=integer4, int2=integer4))
viewFormat(hexViewFile("rawTest.int"),
           memFormat(integers=vectorBlock(integer4, 20)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
\keyword{ connection }
