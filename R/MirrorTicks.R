

## -----------------------------------------------------------------------------

#' GGMirrorTicks
#'
#' Adds mirrored tickmarks in ggplot panels. Especially when faceting where dup_axis() does not work (otherwise works well for single panel plots). Based on example provided by Sandy Muspratt on Stackoverflow: http://stackoverflow.com/a/29023682/143476.
#'
#' @param ggobj output of \code{ggplot}
#' @param sides any of "bltr"
#'
#' @return \code{gtable} object to be drawn with \code{grid.draw}.
#' @export

GGMirrorTicks <- function(ggobj, sides="bltr") {
  ## ggobj : output of ggplot()
  ## slide : which side

  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(gtable)

  gt <- ggplotGrob(ggobj)

  ticklength <- ggobj$theme$axis.ticks.length
  if(is.null(ticklength)) # default
    ticklength <- unit(-0.25, "lines")

  pattern <- c(panel="panel(-([1-9])-([1-9]))?")
  ## single                : "panel"
  ## facet_grid/facet_wrap : "panel-%d-%d"

  template <- list(
    bottom = list(
      axis     ="b",
      grobnum  = 1,
      adjust   = function(ax, len) ax,
      add      = gtable_add_rows,
      pos      = function(panel) -1,
      extent   = function(panel) with(panel, list(t=t+1, l=l, r=r))
    ),
    top = list(
      axis     ="b",
      grobnum  = 1,
      adjust   = function(ax, len) { ax$y <- ax$y + len; ax },
      add      = gtable_add_rows,
      pos      = function(panel) panel$t-1,
      extent   = function(panel) with(panel, list(t=t, l=l, r=r))
    ),
    left = list(
      axis     ="l",
      grobnum  = 2,
      adjust   = function(ax, len) ax,
      add      = gtable_add_cols,
      pos      = function(panel) -1,
      extent   = function(panel) with(panel, list(t=t, l=l-1))
    ),
    right = list(
      axis     ="l",
      grobnum  = 2,
      adjust   = function(ax, len) { ax$x <- ax$x + len; ax },
      add      = gtable_add_cols,
      pos      = function(panel) panel$r,
      extent   = function(panel) with(panel, list(t=t, l=r+1))
    )
  )

  Addticks <- function(gt, panelname, where) {
    ## gt        : gtable object
    ## panelname : name of panel
    ## axnum     : axis number
    ## where     : one of "bottom", "top", "left", "right"
    ## ticklength, template are lexically scoped

    ## Get panel
    panel <-c(subset(gt$layout, name == panelname, se=t:r))
    rownum <- as.integer(sub(pattern["panel"], "\\2", panelname))
    colnum <- as.integer(sub(pattern["panel"], "\\3", panelname))

    ## Get axis grobs
    elems <- template[[pmatch(where, names(template))]]
    if(is.na(rownum) && is.na(colnum)) {
      ## *** single panel ***
      axisname <- sprintf("axis-%s", elems$axis)
      igrob <- which(gt$layout$name == axisname)
    } else {
      ## *** facet_grid ***
      axnum <- ifelse(grepl("b|t", where), rownum, colnum)
      axisname <- sprintf("axis-%s-%d", elems$axis, axnum)
      igrob <- which(gt$layout$name == axisname)
      if(length(igrob)==0) {
        ## *** facet_wrap ***
        axisname <- sprintf("axis-%s-%d-%d", elems$axis, rownum, colnum)
        igrob <- which(gt$layout$name == axisname)
        if(length(igrob)==0) { # not sure what's going on here
          axisname <- sprintf("axis-%s-%d-%d", elems$axis, colnum, rownum)
          igrob <- which(gt$layout$name == axisname)
        }
      }
    }

    axis.grob <- gt$grobs[[igrob]]
    axis.child <- axis.grob$children[[2]]

    ## Get the tick marks
    axticks <- axis.child$grobs[[elems$grobnum]]
    axticks <- do.call(elems$adjust, list(axticks, ticklength))

    ## Add a new column to gt, and insert the revised axis grob into the new column.
    ext <- do.call(elems$extent, list(panel))
    pos <- do.call(elems$pos, list(panel))
    gt <- do.call(elems$add, list(gt, unit(0, "lines"), pos))
    gt <- do.call(gtable_add_grob, c(list(gt, axticks), ext, list(name = "ticks")))

    ## Return
    gt

  }

  panels <- grep(pattern["panel"], gt$layout$name, value=TRUE)

  for(.panelname in panels) {

    for(.side in c("b", "t", "l", "r")) {

      if(grepl(.side, sides)) {

        gt <- tryCatch(Addticks(gt, .panelname, .side), error=function(e) gt)

      }

    }
  }

  ## Turn clipping off
  gt$layout[gt$layout$name == "ticks", ]$clip <- "off"

  ## Return
  gt

}

## -----------------------------------------------------------------------------
## examples:
if(FALSE) {

  set.seed(1)
  df <- data.frame(x=1:10, y=1:10, letter=sample(letters[1:2], 10, replace=TRUE))

  mytheme <- theme(
    axis.text.x       = element_text(size=12, vjust=1, margin=margin(t=8)),
    axis.text.y       = element_text(size=12, angle=90, hjust=0.5, vjust=0, margin=margin(r=8)),
    axis.ticks.length = unit(-0.3, "lines"),
    panel.border      = element_rect(color=1, fill=NA, size=.8),
    panel.background  = element_rect(fill="white"),
    panel.grid        = element_blank()
  )

  ## -----------------------------------------------------------------------------

  ggp <- ggplot(df)+
    geom_point(aes(x,y))+
    mytheme

  gt <- MirrorTicks(ggp)

  grid.draw(gt)

  ggp+
    scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))+
    scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))

  ## -----------------------------------------------------------------------------

  ggp <- ggplot(df)+
    geom_point(aes(x,y))+
    facet_wrap(~letter, scale="free")+
    mytheme

  ggp+
    scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))+
    scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))

  gt <- MirrorTicks(ggp)
  grid.draw(gt)

  ## -----------------------------------------------------------------------------

  ggp <- ggplot(df)+
    geom_point(aes(x,y))+
    facet_wrap(~letter)+
    mytheme

  ggp+
    scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))+
    scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))

  gt <- MirrorTicks(ggp) # doesn't work...
  grid.draw(gt)

  ## -----------------------------------------------------------------------------

  ggp <- ggplot(df)+
    geom_point(aes(x,y))+
    facet_grid(.~letter)+
    mytheme

  ggp+
    scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))+
    scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL))

  gt <- MirrorTicks(ggp)
  grid.draw(gt)

}
## -----------------------------------------------------------------------------
