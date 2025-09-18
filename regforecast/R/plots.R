# R/plots.R
# Diagnostics plot

#' Plot diagnostics: residual ACF, residual series, actual vs forecast
#' @param fit Arima model
#' @param fc  forecast object
#' @param combined data.frame with date, value, type
#' @param file_name output png path
#' @return invisible NULL
#' @export
#' @importFrom forecast ggAcf
#' @importFrom ggplot2 ggplot aes geom_line geom_hline labs theme_minimal theme element_text ggsave
#' @importFrom gridExtra grid.arrange
#' @importFrom stats residuals
plot_diagnostics <- function(fit, fc, combined, file_name = "diagnostics.png") {
  acf_gg <- forecast::ggAcf(stats::residuals(fit), lag.max = 36) +
    ggplot2::labs(title = "ACF of Residuals") + ggplot2::theme_minimal()

  res_df <- data.frame(date = combined$date[combined$type == "Actual"], resid = stats::residuals(fit))
  res_gg <- ggplot2::ggplot(res_df, ggplot2::aes(date, resid)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Residuals over Time") +
    ggplot2::theme_minimal()

  fc_gg <- ggplot2::ggplot(combined, ggplot2::aes(date, value, colour = type)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = "Actual vs Forecast") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")

  g <- gridExtra::grid.arrange(acf_gg, res_gg, fc_gg, ncol = 1)
  ggplot2::ggsave(file_name, g, width = 7, height = 12, dpi = 300)
  cat("-> Diagnostics saved ->", file_name, "\n")
  invisible(NULL)
}
