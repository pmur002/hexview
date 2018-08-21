\name{NEWS}
\title{NEWS file for the hexView package}
\encoding{UTF-8}

\section{Changes in version 0.3-4}{
  \itemize{
    \item Allow \code{switch()} function for \code{markedBlock()} to
    return \code{NULL} (in which case the markedBlock only returns the
    marker).
    \item Include \code{Date} column in result if new \code{time.stamp}
    argument to \code{readEViews()} is \code{TRUE}.  Code contributed
    by Chao Bian.
    \item Fix for \code{nbytes} value returned by \code{readFormat()}.
    \item Added support for \code{integer3} memory blocks.
    \item Added arguments to \code{lengthBlock()} and
    \code{markedBlock()} so that the user can specify labels for
    the results of reading in these memory blocks.
  }
}
