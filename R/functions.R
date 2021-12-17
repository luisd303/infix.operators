#' @name functions
#'
#' @title Utility Functions
#'
#' @section Utility Functions: \describe{
#'     \item{\code{\link{make_names}}}{make_names (see \code{?"make_names"}).}
#' }
NULL

#' @name make_names
#' @aliases make_names
#'
#' @title Make names
#'
#' @description Analoguous to make.names, but replaces the dot with an underscore.
#'
#' @param x Vector of names.
#' @examples
#' make_names(c('FOO', 'bAR&', 'FOO BAR')) # returns c('foo', 'bar_', 'foo_bar')
#' @export
make_names <- function(x) make.names(gsub('_+', '_', gsub(' |[[:punct:]]', '_', tolower(x))), unique = T)
