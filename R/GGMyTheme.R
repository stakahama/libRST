
#' GGMyTheme
#'
#' My theme for ggplot.
#'
#' @return ggplot theme.
#' @export

GGMyTheme <- function() {
  library(ggplot2)
  theme(
    strip.text        = element_text(size=14),
    strip.text.x      = element_text(vjust=1),
    strip.text.y      = element_text(vjust=.5, angle=90), #vjust=0
    strip.background  = element_rect(color=NA, fill=NA, linetype =0),
    axis.title        = element_text(size=14),
    axis.text.x       = element_text(size=12, vjust=1, margin=margin(t=8)),
    axis.text.y       = element_text(size=12, angle=90, hjust=0.5, vjust=0, margin=margin(r=8)),
    axis.ticks        = element_line(lineend = "butt", size=.5),
    axis.ticks.length = unit(-0.3, "lines"),
    panel.border      = element_rect(color=1, fill=NA, size=.8),
    panel.background  = element_rect(fill="white"),
    panel.grid        = element_blank(),
    legend.text       = element_text(size=12),
    legend.title      = element_text(size=14),
    legend.key        = element_rect(fill=NA)
  )
}
