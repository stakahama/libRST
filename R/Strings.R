
#' Capitalize
#'
#' Capitalize strings.
#'
#' @param x character string
#'
#' @return Capitalized character string.
#' @export

Capitalize <- function(x)
  `substr<-`(x, 1, 1, value = toupper(substr(x, 1, 1)))

#' StripWhite
#'
#' Strip white characters.
#'
#' @param x character string
#'
#' @return Character string stripped of white characters.
#' @export

StripWhite <- function(x)
  gsub("^[ ]+|[ ]+$","",x)

#' Recsub
#'
#' Recursive string substitution.
#'
#' @param string character string to replace
#' @param replace list of string pairs to be passed to \code{gsub}
#'
#' @return Character string, same length as \code{string}.
#' @export

Recsub <- function(string, replace, ...)
  ## recursive substitution
  ## example:
  ## recsub(string, c("old"="new", "old2"="new2")
  Reduce(function(s,i) gsub(names(replace)[i], replace[i], s, ...),
         seq_along(replace), string)
