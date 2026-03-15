#-------------------------------------------------------------------------------
#' Column as a Named Vector
#' @name as_named_vec
#' @description
#' Converts `data.frame` column into a vector using another column as the names for the vector
#' @param df `data.frame` used.
#' @param val Column name of values
#' @param name Column value of names
#' @returns vector with values `val` and names `name`
#'
#' @keywords internal
as_named_vec <- function(df, val, name) {
  x <- df[[val]]
  names(x) <- df[[name]]
  x
}
