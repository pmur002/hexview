
# All blocks inherit from "memBlock"
is.memBlock <- function(x) {
    inherits(x, "memBlock")
}

#########
# Block definitions
#########

memBlock <- function(nbytes=1, width=NULL, machine="hex") {
    block <- list(nbytes=nbytes, width=width, machine=machine,
                  endian="little", signed=TRUE)
    class(block) <- "memBlock"
    block
}

# Single basic type
atomicBlock <- function(type="char", width=NULL, machine="hex",
                        # R defaults
                        size=switch(type, char=1, int=4, real=8),
                        endian="little", signed=TRUE) {
    block <- list(type=type, width=width, machine=machine,
                  size=size, endian=endian, signed=signed)
    class(block) <- c("atomicBlock", "memBlock")
    block
}

ASCIIchar <- atomicBlock()
integer1 <- atomicBlock("int", size=1)
integer2 <- atomicBlock("int", size=2)
integer4 <- atomicBlock("int")
integer8 <- atomicBlock("int", size=8)
real4 <- atomicBlock("real", size=4)
real8 <- atomicBlock("real")

## Needs special treatment because not natively supported
## (in most places)
integer3 <- function(width=NULL, machine="hex",
                     endian="little", signed=TRUE) {
    block <- list(type="int", width=width, machine=machine,
                  size=3, endian=endian, signed=signed)
    class(block) <- c("integer3", "atomicBlock", "memBlock")
    block
}
    
ASCIIline <- list()
class(ASCIIline) <- c("ASCIIlineBlock", "memBlock")

# Fixed length sequence of basic type
vectorBlock <- function(block=ASCIIchar,
                        length=1) {
    block <- list(block=block, length=length)
    class(block) <- c("vectorBlock", "memBlock")
    block
}

# Vector block preceded by a block which gives the length of the vector
lengthBlock <- function(length=integer4,
                        block=ASCIIchar,
                        blockLabel="block") {
    block <- list(block=block, length=length,
                  blockLabel=blockLabel)
    class(block) <- c("lengthBlock", "memBlock")
    block    
}

mixedBlock <- function(...) {
    block <- list(...)
    class(block) <- c("mixedBlock", "memBlock")
    block    
}

# A block marker encodes the block to follow
# The switch function decodes the marker and determines the block
markedBlock <- function(marker=integer4,
                        switch=function(marker) { ASCIIchar },
                        markerLabel="marker", blockLabel="block") {
    block <- list(marker=marker, switch=switch,
                  markerLabel=markerLabel, blockLabel=blockLabel)
    class(block) <- c("markedBlock", "memBlock")
    block    
}

#########
# Reading blocks from files
#########

readBlock <- function(block, file) {
    # If called directly with file name, open and close file
    # Otherwise, assume it is an *open* connection
    if (is.character(file)) {
        file <- file(file, "rb")
        on.exit(close(file))
    }

    # Just query current file position
    offset <- seek(file, where=NA)
    
    readMemBlock(block, file, offset)
}

readMemBlock <- function(block, file, offset) {
    UseMethod("readMemBlock")
}

readMemBlock.memBlock <- function(block, file, offset) {
    with(block,
         readRawBlock(file, width, machine, "char", offset, nbytes,
                      1, "little", FALSE))
}

readMemBlock.atomicBlock <- function(block, file, offset) {
    with(block,
         readRawBlock(file, width, machine, type, offset, size,
                      size, endian, signed))
}

readMemBlock.integer3 <- function(block, file, offset) {
    ## Read as char ...
    rawBlock <- with(block,
                     readRawBlock(file, width, machine, "char", offset, size,
                                  size, endian, signed))
    ## ... then reset type and fill in numeric value
    rawBlock$type <- "int"
    ## Check sign
    sign <- readBin(rawBlock$fileRaw, "integer", size=1)
    if (sign < 0) {
        ## Pad with FF
        rawBlock$fileNum <- readBin(c(as.raw(2^8 - 1), rawBlock$fileRaw),
                                    "integer", size=4, endian=block$endian)
    } else {
        ## Pad with 00
        rawBlock$fileNum <- readBin(c(as.raw(0), rawBlock$fileRaw),
                                    "integer", size=4, endian=block$endian)
    }
    rawBlock
}

readMemBlock.ASCIIlineBlock <- function(block, file, offset) {
    readASCIIline(file, offset)
}

readVectorBlock <- function(block, length, file, offset) {
    UseMethod("readVectorBlock")
}

# A vector of "atomic" blocks is easy
readVectorBlock.atomicBlock <- function(block, length, file, offset) {
    with(block,
         readRawBlock(file, width, machine, type, offset, length*size,
                      size, endian, signed))
}

# A vector of NOT "atomic" blocks is less straightforward
readVectorBlock.default <- function(block, length, file, offset) {
    result <- vector("list", length)
    for (i in 1:length) {
        result[[i]] <- readBlock(block, file)
    }
    result
}

readMemBlock.vectorBlock <- function(block, file, offset) {
    readVectorBlock(block$block, block$length, file, offset)
}

readMemBlock.lengthBlock <- function(block, file, offset) {
    lengthBlock <- readBlock(block$length, file)
    # Update file location
    offset <- seek(file, where=NA)
    vecBlock <- readVectorBlock(block$block, blockValue(lengthBlock),
                                file, offset)
    result <- list(lengthBlock, vecBlock)
    names(result) <- c("length", block$blockLabel)
    result
}

readMemBlock.mixedBlock <- function(block, file, offset) {
    lapply(block, readBlock, file)
}

readMemBlock.markedBlock <- function(block, file, offset) {
    markerBlock <- readBlock(block$marker, file)
    nextBlock <- block$switch(markerBlock)
    if (is.null(nextBlock)) {
        result <- list(markerBlock)
        names(result) <- block$markerLabel
    } else {
        markedBlock <- readBlock(nextBlock, file)
        result <- list(markerBlock, markedBlock)
        names(result) <- c(block$markerLabel, block$blockLabel)
    }
    result
}

