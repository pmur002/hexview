
# User functions for creating and viewing rawFormat's

readFormat <- function(file, ...) {
    UseMethod("readFormat")
}

readFormat.character <- function(file,
                                 format,
                                 width=NULL, offset=0, 
                                 machine="hex",
                                 flatten=TRUE) {
    infile <- file(file, "rb")
    on.exit(close(infile))

    readFormat(infile, format, width, offset, machine, flatten)
}

readFormat.connection <- function(file,
                                  format,
                                  width=NULL, offset=0, 
                                  machine="hex",
                                  flatten=TRUE) {
    if (!is.memFormat(format))
        stop("Invalid format")

    if (offset > 0)
        seek(file, offset)

    blocks <- lapply(format, readBlock, file)
    fileFormat <- list(blocks=blocks)
    if (flatten) {
        fileFormat$blocks <- flattenFormat(fileFormat$blocks)
        class(fileFormat) <- c("flatRawFormat", "rawFormat")
    } else {
        class(fileFormat) <- "rawFormat"
    }
    fileFormat$offset <- offset
    fileFormat$nbytes <- seek(file) - offset
    fileFormat    
}

viewFormat <- function(..., page=FALSE) {
    print(readFormat(...), page=page)
}
