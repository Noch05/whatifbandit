#' Verbose Printer
#' @description Shorthand Function for checking `verbose` and then printing. Takes verbose from higher scope
#' @name verbose_log
#' @param message The message to be printed to screen, as a string.
#' @param log Logical; Whether or not to print the message

verbose_log <- function(log, message) {
  if (log) {
    base::cat(message, "\n")
  }
}
