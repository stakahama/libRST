#' Subsetinrange
#'
#' Helper function. Returns values falling within a predetermined interval.
#'
#' @param x numeric vector
#' @param lim interval limits
#'
#' @return Selected values.

Subsetinrange <- function(x, lim)
  x[findInterval(x, sort(lim), rightmost.closed = TRUE) == 1]

#' Parusrlim
#'
#' Helper function. Returns axis limits.
#'
#' @param side 1-4
#'
#' @return User limits for selected side.

Parusrlim <- function(side)
  par("usr")[(1:2) + 2 * (((side - 1) %% 2 + 1) - 1)]

#' LogAxis
#'
#' Logarithmic axis
#'
#' @param side 1-4
#' @param labels TRUE/FALSE If FALSE, add minor tick labels like Igor Pro
#' @param minor TRUE/FALSE
#' @param decades.only TRUE/FALSE
#' @param major.labels TRUE/FALSE
#' @param minor.labels TRUE/FALSE
#' @param major.args list of tick arguments to \code{axis}
#' @param minor.args list of tick arguments to \code{axis}
#' @param ... to be passed to \code{axis}
#'
#' @return List of tick positions and labels.
#' @export

LogAxis <-
  function(side,
           labels = TRUE,
           minor = TRUE,
           decades.only = TRUE,
           major.labels = labels,
           minor.labels = FALSE,
           major.args = NULL,
           minor.args = NULL,
           ...) {
    logtest <- par(c("xlog", "ylog")[(side - 1) %% 2 + 1])
    if (!logtest) {
      warning("not log scale")
      at <- axTicks(side)
      reform <-
        (at > .Machine$double.eps | -at >= .Machine$double.neg.eps)
      if (any(reform)) {
        vals <- at[reform]
        tens <- floor(log10(vals))
        units <- round(vals / (10 ^ tens))
        labels <-
          parse(text = replace(at, reform, sprintf("%d %%*%% 10^%d", units, tens)))
      } else {
        labels <- format(at)
      }
      do.call(axis, c(list(
        side = side,
        at = at,
        labels = labels
      ), list(...)))
    } else {
      if (!"at" %in% names(major.args)) {
        vals <- axTicks(side)
        tens <- floor(log10(vals))
        at <- (if (decades.only)
          10 ^ unique(tens)
          else
            with(list(units = round(
              vals / (10 ^ tens)
            )),
            unique(units * 10 ^ tens)))
      } else {
        at <- vals <- tens <- major.args[["at"]]
        major.args[["at"]] <- NULL
      }
      if (labels)
        labels <-
          (if (decades.only)
            parse(text = sprintf("10^%d", log10(at)))
           else
             with(list(u = at, d = floor(log10(
               at
             ))),
             parse(text = sprintf(
               "%d %%*%% 10^%d", round(u / (10 ^ d)), d
             ))))
      do.call(axis, c(list(
        side = side,
        at = at,
        labels = labels
      ), major.args))
      if (minor) {
        tens <- do.call(`:`, as.list(range(round(log10(
          vals
        ))) + c(-1, 1)))
        minor.at <- outer(1:9, 10 ^ tens)
        if (is.null(minor.args$tck))
          minor.args$tck <- -0.01
        do.call(axis, c(
          list(
            side = side,
            at = Subsetinrange(minor.at, 10 ^ Parusrlim(side)),
            labels = FALSE
          ),
          minor.args
        ))
        if (minor.labels) {
          at.minor <- Subsetinrange(minor.at[-1, ], 10 ^ Parusrlim(side))
          lab.minor <-
            substring(formatC(at.minor, digits = 1, format = "e"), 1, 1)
          do.call(mtext, c(
            list(
              lab.minor,
              side = side,
              at = at.minor,
              line = 0,
              cex = .6
            ),
            minor.args
          ))
        }
      }
    }
    invisible(list(at = at, labels = labels))
  }

#' TimeAxis
#'
#' Adds a chron axis.
#'
#' @param side
#' @param major.interval
#' @param minor.interval
#' @param format
#' @param offset
#' @param lim
#' @param major.extend
#' @param labels
#' @param major.args
#' @param minor.args
#' @param origin
#' @param ... passed to \code{par}()
#'
#' @return Vector of tick positions.
#' @export

TimeAxis <-
  function (side = 1,
            major.interval = 7,
            minor.interval = 1,
            format = "%m/%d",
            offset = 0,
            lim = Parusrlim(side),
            major.extend = c(-1, 1),
            labels = TRUE,
            major.args = NULL,
            minor.args = NULL,
            origin = "1970-01-01 00:00:00",
            ...) {
    library(chron)
    ## for use with chron
    ## can also pass 'lend' to ... which will be passed to par
    force(origin)
    force(format)
    dotArgs <- list(...)
    formatfn <- function(x)
      base::format(as.POSIXct(x * 86400., tz = "GMT", origin = origin),
                   format = format)
    nearest <- function(x, y, fn)
      fn(x / y) * y
    increment <- major.interval
    extend <- major.interval * major.extend
    nearlim <- with(list(x = unclass(lim), y = increment),
                    c(nearest(min(x), y, floor),
                      nearest(max(x), y, ceiling))[order(x)])
    major.pos <-
      do.call(seq, c(as.list(nearlim + extend), by = major.interval)) + offset
    major.at <- Subsetinrange(major.pos, lim)
    major.labels <- if (labels)
      formatfn(major.at)
    else
      labels
    if (!is.null(dotArgs$lheight)) {
      lheight <- par("lheight")
      par(lheight = dotArgs$lheight)
    }
    do.call(axis, c(
      list(
        side = side,
        at = major.at,
        labels = major.labels
      ),
      major.args
    ))
    if (exists("lheight"))
      par(lheight = lheight)
    tickpos <- list(major = major.at)
    if (length(minor.interval) != 0 && !is.na(minor.interval)) {
      minor.pos <-
        do.call(seq, c(as.list(nearlim + extend), list(by = minor.interval)))
      minor.at <- minor.pos
      if (is.null(minor.args$tck))
        minor.args$tck <- sign(par("tck")) * 0.01
      do.call(axis, c(list(
        side = side,
        at = minor.at,
        labels = FALSE
      ), minor.args))
      tickpos$minor <- minor.at
    }
    invisible(tickpos)
  }

#' ColAlpha
#'
#' Add alpha transparency to colors.
#'
#' @param colvec
#' @param alpha
#' @param maxColorValue
#'
#' @return Output of \code{rgb}.
#' @export

ColAlpha <- function(colvec,
                     alpha = 0.5,
                     maxColorValue = 255)
  rgb(t(col2rgb(colvec)) / maxColorValue, alpha = alpha)


#' MTitle
#'
#' Like \code{title} but places labels in outer margin.
#'
#' @param main
#' @param xlab
#' @param ylab
#' @param ylab2
#' @param line
#' @param line.frac
#'
#' @return \code{NULL}
#' @export

MTitle <-
  function(main = NULL,
           xlab = NULL,
           ylab = NULL,
           ylab2 = NULL,
           line = NULL,
           line.frac = 2 / 5,
           ...) {
    if (length(line.frac) < 2)
      line.frac <- rep(line.frac, 4)
    oma <- function(i)
      if (par("oma")[i] > 0)
        par("oma")[i]
    else
      par("mar")[i]
    lineval <- function(i)
      if (is.null(line))
        oma(i) * line.frac[i]
    else
      line
    if (!is.null(xlab))
      mtext(xlab, 1, outer = TRUE, line = lineval(1), ...)
    if (!is.null(ylab))
      mtext(ylab, 2, outer = TRUE, line = lineval(2), ...)
    if (!is.null(main))
      mtext(main, 3, outer = TRUE, line = lineval(3), ...)
    if (!is.null(ylab2))
      mtext(ylab2, 4, outer = TRUE, line = lineval(4), ...)
    NULL
  }

#' TextLabel
#'
#' Convenience functions for adding letters and labels to figures. Adds padding not provided by \code{text(par("usr")[1], par("usr")[4], ...)}.
#'
#' @param location one of "topleft", "topright", "bottomleft", "bottomright"
#' @param ... to \code{text}
#' @param yoffset generally integer values to be multipliedy by \code{par("cxy")}
#' @param expansion
#'
#' @return \code{NULL}
#' @export


TextLabel <- function(location,
                      ...,
                      yoffset = 0,
                      expansion = .1) {
  x <- if (grepl("left$", location)) {
    list(i = 1, op = `+`, adj = 0)
  } else if (grepl("right$", location)) {
    list(i = 2, op = `-`, adj = 1)
  }
  x$pos <- if (par("xlog"))
    10 ^ par("usr")[x$i]
  else
    par("usr")[x$i]
  y <- if (grepl("^bottom", location)) {
    list(i = 3, op = `+`, adj = 0)
  } else if (grepl("^top", location)) {
    list(i = 4, op = `-`, adj = 1)
  }
  y$pos <- if (par("ylog"))
    10 ^ par("usr")[y$i]
  else
    par("usr")[y$i]
  text(x$op(x$pos, par("cxy")[1] * expansion),
       y$op(y$op(y$pos, par("cxy")[2] * expansion * 2), par("cxy")[2] *
              yoffset),
       adj = c(x$adj, y$adj),
       ...)
  NULL
}
