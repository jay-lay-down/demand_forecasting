# R/backtest.R
# Backtest helpers: leak-free exogenous forecasting and plotting

#' Run leak-free backtest from a given year-month
#'
#' Fits SARIMAX on the training window that ends at (start_year, start_month-1),
#' builds xreg using only information up to the training end (no leakage),
#' and forecasts over the holdout period.
#'
#' @param y_ts ts (log scale target)
#' @param ex_ts ts (exogenous series, observed history)
#' @param shift numeric shift used in log(x + shift) for inverse transform
#' @param start_year integer, e.g. 2023
#' @param start_month integer in 1..12
#' @param L integer lag for y_t ~ x_{t-L}
#' @return data.frame with columns: date<Date>, Actual, Forecast
#' @export
#' @importFrom stats window start
#' @importFrom forecast auto.arima forecast
run_backtest_from <- function(y_ts, ex_ts, shift, start_year = 2023, start_month = 1, L = 1) {
  train_end <- c(start_year, start_month - 1)
  if (train_end[2] == 0) train_end <- c(start_year - 1, 12)

  xbt <- build_xreg_backtest(y_ts, ex_ts, L = L, train_end = train_end)
  if (is.null(xbt)) return(NULL)

  y_tr <- stats::window(y_ts, end = train_end)
  fit  <- forecast::auto.arima(y_tr, xreg = xbt$x_tr,
                               seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  fc   <- forecast::forecast(fit, h = xbt$h_bt, xreg = xbt$x_te)

  ts_start <- stats::start(y_ts)
  d_all <- seq(as.Date(sprintf("%04d-%02d-01", ts_start[1], ts_start[2])),
               by = "month", length.out = length(y_ts))
  d_bt  <- d_all[(xbt$Ttr + 1):(xbt$Ttr + xbt$h_bt)]

  out <- data.frame(
    date     = as.Date(d_bt),
    Actual   = invlog(y_ts[(xbt$Ttr + 1):(xbt$Ttr + xbt$h_bt)], shift),
    Forecast = invlog(as.numeric(fc$mean), shift)
  )
  out
}

#' Plot backtest Actual vs Forecast
#'
#' @param bt_df data.frame with columns: date<Date>, Actual, Forecast
#' @param pg_name character label for title
#' @param file_name output PNG path
#' @return invisible NULL
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line scale_colour_manual scale_x_date labs theme_minimal theme element_text ggsave
plot_backtest <- function(bt_df, pg_name, file_name) {
  if (is.null(bt_df) || nrow(bt_df) == 0) return(invisible(NULL))
  stopifnot(inherits(bt_df$date, "Date"))

  df_long <- tidyr::pivot_longer(
    bt_df, cols = c("Actual", "Forecast"),
    names_to = "type", values_to = "value"
  )
  df_long$type <- factor(df_long$type, levels = c("Actual", "Forecast"))

  p <- ggplot2::ggplot(df_long, ggplot2::aes(date, value, colour = type)) +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::scale_colour_manual(values = c(Actual = "#1f77b4", Forecast = "#ff7f0e"),
                                 breaks = c("Actual", "Forecast"),
                                 limits = c("Actual", "Forecast"),
                                 drop = FALSE) +
    ggplot2::scale_x_date(date_labels = "%Y-%m") +
    ggplot2::labs(title = paste0("Backtest (from 2023) — ", pg_name),
                  x = "Date", y = "Value", colour = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top",
                   plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(file_name, p, width = 8, height = 4.5, dpi = 300)
  cat(" -> Backtest plot saved:", file_name, "\n")
  invisible(NULL)
}
