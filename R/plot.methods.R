#' Plot Generics for multiple.mab object
#' @description
#' Generic R plot summarizing AIPW estimate across trials.
#'
#' @method plot multiple.mab
#' @inheritParams gg_mab
#' @param ... further arguments passed to or from other methods
#' @export
#' @returns Box plot of the AIPW Mean Across all Trials.

plot.multiple.mab <- function(x, ...) {
  estimates <- x$estimates
  graphics::boxplot(mean ~ mab_condition, estimates,
    xlab = "Treatment Condition",
    ylab = "AIPW Estimate"
  )
}

#' @title ggplot2 Generic for multible.mab Object
#' @name gg_mab
#' @description
#' [ggplot2] plot summarizing AIPW estimate across trials.
#' @param x "multiple.mab" class object created by [multiple_mab_simulation()]
#' @export
#' @returns Box plot of the AIPW Mean Across all Trials using [ggplot2]
gg_mab <- function(x) {
  estimates <- x$estimates
  ggplot2::ggplot(estimates, ggplot2::aes(x = mab_condition, y = mean)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Treatment Condition",
      y = "AIPW Estimate"
    )
}
