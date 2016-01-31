
EViewsHeader <- function() {
    memFormat(firstline=memBlock(80),
              headersize=integer8,
              unknown=memBlock(26),
              numvblesplusone=integer4,
              date=vectorBlock(ASCIIchar, 4),
              unkown=memBlock(2),
              datafreq=integer2,
              startperiod=integer2,
              startobs=integer4,
              unkown=memBlock(8),
              numobs=integer4)
}

EViewsVbleInfo <- function() {
    memFormat(unknown=memBlock(6),
              recsize=integer4,
              memsize=integer4,
              ptrtodata=integer8,
              vblename=vectorBlock(ASCIIchar, 32),
              ptrtohistory=integer8,
              vbletype=integer2,
              unknown=memBlock(6))
}

EViewsVbleData <- function(numObs) {
    memFormat(numobs=integer4,
              startobs=integer4,
              unknown=memBlock(8),
              endobs=integer4,
              unknown=memBlock(2),
              values=vectorBlock(real8, 
                numObs))
}

readEViews <- function(filename, time.stamp=TRUE, as.data.frame=TRUE) {
    # Start with the header information
    header <- readFormat(filename, EViewsHeader())
    vbleStart <- blockValue(header$blocks$headersize) + 24 + 2
    # Extract time series labels from data
    if (time.stamp) {
        startobs <- blockValue(header$blocks$startobs)
        startperiod <- blockValue(header$blocks$startperiod)
        datafreq <- blockValue(header$blocks$datafreq)
        if ( !is.numeric(startobs) ||
             !is.numeric(startperiod) ||
             !is.numeric(datafreq) ) {
            warning("Can not extract time series information. Setting time.stamp to FALSE\n")
            time.stamp <- FALSE
        }
    }
    # Going to ignore "C" and "RESID"
    numVbles <- blockValue(header$blocks$numvblesplusone) - 1 - 2
    numObs <- blockValue(header$blocks$numobs)
    # Empty container for data set
    # Add 'Date' column if time.stamp is turned on.
    if (time.stamp) {
        vbleCount <- 2
        Names <- vector("character", numVbles+1)
        Data <- vector("list", numVbles+1)
    } else {
        vbleCount <- 1
        Names <- vector("character", numVbles)
        Data <- vector("list", numVbles)
    }
    for (i in 1:(numVbles + 2)) {
        vbleInfo <- readFormat(filename,
                               offset=vbleStart + (i - 1)*70,
                               EViewsVbleInfo())
        vbleName <- blockString(vbleInfo$blocks$vblename)
        if (vbleName == "C" || vbleName == "RESID") {
            warning("Skipping boilerplate variable\n")
        } else {
            Names[vbleCount] <- vbleName
            dataLoc <- blockValue(vbleInfo$blocks$ptrtodata)
            vbleValues <- readFormat(filename,
                                     offset=dataLoc,
                                     EViewsVbleData(numObs))
            Data[[vbleCount]] <- blockValue(vbleValues$blocks$values)
            vbleCount <- vbleCount + 1
        }
    }
    # Add time stamp to data
    if (time.stamp) {
        start.date <-
            switch(as.character(datafreq),
                   "1" =as.Date(paste(startobs, "1", "1", sep="-")),
                   "4" =as.Date(paste(startobs, startperiod*3 - 2, "1",
                                      sep="-")),
                   "12"=as.Date(paste(startobs, startperiod, "1", sep="-"))
                   )
        increment <-
            switch(as.character(datafreq),
                   "1" ="year",
                   "4" ="quarter",
                   "12"="month"
                   )
        Names[1]  <- "Date"
        Data[[1]] <- seq(start.date, by = increment, length.out = numObs)
    }
    names(Data) <- Names
    if (as.data.frame)
        as.data.frame(Data)
    else
        Data
}
