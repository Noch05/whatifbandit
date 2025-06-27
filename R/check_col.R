#' @title Checking Existence of a Column in data.frame
#' @name check_col
#'
#'
#' @description Helper function to [check_args()]. Checks whether the specified column
#' exists in the data provided.
#'
#' @param data input data.frame
#' @param col column to check if exists
#'
#' @returns TRUE or FALSE based on the existence of the column in the data.
#'
#' @seealso
#' *[single_mab_simulation()]
#' *[check_args()]
#'
#' @export
#'
check_col <- function(data, col) {
  if (rlang::quo_is_null(rlang::enquo(col))) {
    return(FALSE)
  }
  name <- rlang::as_name(rlang::enquo(col))

  return(name %in% base::names(data))
}
