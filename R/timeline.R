library(lubridate)

#' Plot a historical timeline, with time on the vertical axis
#'
#' The time axis appears on the left of the diagram, with lines extending from event
#' times to a list of labels that are right-justified at the right of the diagram. The
#' event label sizes are controlled by the \code{cex} argument, but if that value
#' would yield event labels taking more than \code{fraction} of the plot area, then the character
#' size is reduced so that the labels take up no more than \code{fraction} of the area. Adjusting
#' \code{fraction} and plot width may be required to get diagrams that are easily understood.
#'
#' If the \code{file} argument is given, then the \code{time}, \code{event}
#' and \code{col} arguments need not be given, and indeed the values supplied
#' by those arguments will be ignored.
#'
#' @param time Vector of times, or character strings that can be coerced into times
#' using \code{\link[lubridate]{ymd}}, e.g. \code{"2019-06-30"} or \code{"20190630"}
#' for May 30, 2019. The times need not be in order, because they (and the \code{event}
#' and \code{col} values) are all put into temporal order by this function.
#'
#' @param event Vector of (brief) strings indicating what happened at the indicated times.
#' The number of entries must match the number of times.
#'
#' @param col Vector of colours to use for the events. This will be lengthened
#' to match the length of \code{time}.
#'
#' @param labelTime String used to label the time axis. The default is to
#' not label the axis, which saves some margin space to the left of the plot.
#'
#' @param file Optional string giving the name of a comma-separated-value
#' file, to be read by \code{\link{read.csv}}, that possibly contains columns
#' labelled \code{time}, \code{event}, and \code{col}. Any of these
#' columns that are present take precedence over the arguments of those names.
#'
#' @param cex Character expansion factor for event strings. If the longest
#' event string will not fit within half the plot width, then \code{cex}
#' will be reduced to make it fit.
#'
#' @param fraction Maximal fraction of plot area to devote to the event
#' labels.
#'
#' @param debug An integer that indicates whethr to printing some information
#' about the processing. This can be handy if problems arise, but the
#' default is to work silently.
#'
#' @examples
#' # Method 1: read the data (fifty years of ocean-related research)
#' entries <- "time, event
#'1850-01-01,Clausius 2nd law of thermodynamics
#'1851-01-01,Foucault pendulum
#'1856-01-01,Ferrel pressure-gradient & wind velocity
#'1865-01-01,Forchhammer oceanic ion ratio constancy
#'1867-01-01,Kelvin theorem
#'1872-01-01,Challenger expediction
#'1872-01-01,tide prediction machine (Kelvin)
#'1879-01-01,Stefan black-body radiation law
#'1879-01-01,Lamb \"Hydrodynamics\"
#'1881-01-01,Langley radiant heat flux measurement
#'1883-01-01,Reynolds' turbulence analysis
#'1883-01-01,Fram expedition
#'1886-01-01,Vitiaz expedition
#'1889-01-01,Dutton isostacy
#'1890-01-01,Pillsbury Gulf Stream surveys
#'1893-01-01,Fram expedition
#'1898-01-01,Bjerke method of dynamical analysis
#'1901-01-01,Bénard convection
#'1901-01-01,Knudsen seawater tables
#'1902-01-01,Ekman Spiral
#'"
#'
#' d <- read.csv(text=entries, sep=",", header=TRUE)
#' timelineVertical(d$time, d$event)
#'
#' # Method 2: specify a file (normally just a filename; here a connection)
#' timelineVertical(file=textConnection(entries))
#' mtext("Fifty years of ocean-related science")
#'
#' @export
#' @importFrom lubridate ymd
#' @author Dan Kelley
timelineVertical <- function(time, event, col="black", labelTime="", file, cex=par("cex"), fraction=0.5, debug=0)
{
    if (!missing(file)) {
        d <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
        dnames <- names(d)
        time <- if ("time" %in% dnames) d$time else stop("file does not contain a 'time' column")
        event <- if ("event" %in% dnames) d$event else stop("file does not contain a 'event' column")
        col <- if ("col" %in% dnames) d$col else "black"
    }
    ## fix 'col' problem if user read data outside of this function, and forgot stringsAsFactors=FALSE
    col <- as.vector(col)
    n <- length(time)
    if (n < 1)
        stop("no time given")
    if (!inherits(time, "POSIXt")) {
        if (debug > 0)
            cat("creating time from string value to POSIXt value\n")
        time <- ymd(time)
    }
    time <- as.POSIXct(time)
    if (n != length(event))
        stop("length of time (", n, ") does not match length of event (", length(event), ")")
    if (length(col) < n)
        col <- rep(col, length.out=n)
    o <- order(time)
    time <- time[o]
    event <- event[o]
    col <- col[o]
    mar <- c(1.5, if (labelTime == "") 2 else 3.5, 1.5, 1.5)
    mgp <- c(2, 0.7, 0)
    tlim <- range(time)
    par(mar=mar, mgp=mgp)
    if (debug > 0)
        cat("time ranges from", paste(tlim, collapse=" to "), "\n")
    plot(c(0, 1), tlim, type="n", axes=FALSE, xlab="", ylab=labelTime, xaxs="i")
    usr <- par("usr")
    ## oce.axis.POSIXct(2, drawTimeRange=FALSE)
    axis.POSIXct(x=c(time, usr[3], usr[4]), side=2)
    box()

    left <- usr[1]
    right <- usr[1] + (1 - fraction)*(usr[2] - usr[1])
    ## Add a space character at the end of th event strings
    event <- paste(event, " ", sep="")
    ## We will get space for the original event string, plus the trailing space, plus a space before the string.
    space <- "  " # append to event strings, so width accounts for pos=4 in text()
    maxWidth <- max(unlist(lapply(event, function(e) strwidth(paste(space, e), cex=cex))))
    if (debug > 0)
        cat("cex=", cex, ", maxWidth=", maxWidth, ", left=", left, ", right=", right, ", fraction=", fraction, "\n", sep="")
    if (maxWidth > fraction) { #maxWidth > (1 - fraction)) {
        if (debug > 0)
            cat("cex adjusted from", cex, "to ", cex * fraction / maxWidth, "to fit largest event string\n")
        cex <- cex * fraction / maxWidth
    }
    maxWidth <- max(unlist(lapply(event, function(e) strwidth(paste(space, e), cex=cex))))
    if (debug > 0)
        cat("after adjustment, cex=", cex, ", maxWidth=", maxWidth, "\n", sep="")
    y <- seq(tlim[1], tlim[2], length.out=n)
    right <- usr[1] + (1-maxWidth)*(usr[2] - usr[1])
    for (i in seq_len(n)) {
        width <- strwidth(paste(space, event[i]), cex=cex) # add spaces
        if (debug > 0)
            cat(paste("* \"", event[i], "\" at ", time[i], " has width=",width,", col=",col[i],", cex=",cex,"\n", sep=""))
        lines(c(left, right), c(time[i], y[i]), col=col[i])
        text(right, y[i], event[i], pos=4, cex=cex, col=col[i])
    }
}

