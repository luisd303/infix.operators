#' @name operators
#'
#' @title Infix Operators
#'
#' @section Infix Operators: \describe{
#'     \item{\code{\link{coalesce}}}{Coalesce (see \code{?"\%||\%"}).}
#'     \item{\code{\link{paste0}}}{Concatenate (see \code{?"\%+\%"}).}
#'     \item{\code{\link{paste.comma}}}{Concatenate comma separated (see \code{?"\%,\%"}).}
#'     \item{\code{\link{paste.space}}}{Concatenate space separated (see \code{?"\% \%"}).}
#'     \item{\code{\link{file.path}}}{Construct path (see \code{?"\%//\%"}).}
#'     \item{\code{\link{not.in}}}{Negated in (see \code{?"\%!in\%"}).}
#' }
NULL

#' @name coalesce
#' @aliases %||%
#'
#' @title Coalesce
#'
#' @description Right side when left side is \code{NA}
#'
#' @param a,b Returns first non-\code{NA}.
#' @examples
#' 1 %||% 2 # returns 1
#' NA %||% 2 # returns 2
#' @export
`%||%` <- function(a, b) if (is.na(a)) b else a

#' @name paste0
#' @aliases paste0
#' @aliases %+%
#'
#' @title Concatenate
#'
#' @description Equivalent to \code{paste0(x, y)}, but removes \code{NA}.
#'
#' @param x,y Vectors to be concatenated.
#' @examples
#' NA %+% "this" %+% 1900 # returns "this1900"
#' @export
`%+%` <- function(x, y) paste0(as.character(x) %||% '', as.character(y) %||% '')

#' @name paste.comma
#' @aliases paste.comma
#' @aliases %,%
#'
#' @title Concatenate with comma separation
#'
#' @description Equivalent to \code{paste(x, y, sep = ' ')}, but removes \code{NA}.
#'
#' @param x,y Vectors to be concatenated.
#' @examples
#' NA %,% "this" %,% 1900 # returns "this, 1900"
#' @export
`%,%` <- function(x, y) paste0(as.character(x) %||% '', ifelse(is.na(x), '', ', '), as.character(y) %||% '')

#' @name paste.space
#' @aliases paste.space
#' @aliases % %
#'
#' @title Concatenate with space separation
#'
#' @description Equivalent to \code{paste(x, y, sep = ' ')}, but removes \code{NA}.
#'
#' @param x,y Vectors to be concatenated.
#' @examples
#' NA % % "this" % % 1900 # returns "this 1900"
#' @export
`% %` <- function(x, y) paste0(as.character(x) %||% '', ifelse(is.na(x), '', ' '), as.character(y) %||% '')

#' @name file.path
#' @aliases %//%
#'
#' @title Construct Path
#'
#' @description Analogous to \code{file.path(x, y)}.
#'
#' @param x,y Character vectors.
#' @examples
#' "home" %//% "dir" # returns "home/dir"
#' @export
`%//%` <- function(x, y) file.path(x, y)

#' @name not.in
#' @aliases %!in%
#'
#' @title Not in vector
#'
#' @description Negation of \code{\link[base]{\%in\%}}.
#'
#' @param x Vector or \code{NULL}: the values to be (not) matched.
#' @param table Vector or \code{NULL}: the values to be (not) matched against.
#' @examples
#' 4 %!in% 1:3 # returns TRUE
#' @export
`%!in%` <- function(x, table) match(x, table, nomatch = 0L) <= 0L
