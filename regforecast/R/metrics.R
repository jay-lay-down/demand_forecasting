# R/metrics.R
# Accuracy and CV helpers

#' Accuracy metrics including MASE and Theil's U
#' @param actual numeric vector of actuals (original scale)
#' @param pred   numeric vector of predictions (original scale)
#' @param season_lag integer season lag for MASE denominator (default 12)
#' @return data.frame with metrics
#' @export
#' @importFrom stats var
accuracy_tbl <- function(actual, pred, season_lag = 12) {
  mae   <- mean(abs(actual - pred))
  rmse  <- sqrt(mean((actual - pred)^2))
  mape  <- mean(abs((actual - pred) / actual)) * 100
  smape <- mean(2 * abs(pred - actual) / (abs(pred) + abs(actual))) * 100
  r2    <- suppressWarnings(cor(actual, pred, use = "complete.obs")^2)
  mase  <- mean(abs(actual - pred)) / mean(abs(diff(actual, lag = season_lag)), na.rm = TRUE)
  tu    <- sqrt(mean((pred - actual)^2)) / (sqrt(mean(actual^2)) + sqrt(mean(pred^2)))

  data.frame(
    Metric = c("MAE", "RMSE", "MAPE", "sMAPE", "R2", "MASE", "Theil_U"),
    Value  = c(mae, rmse, mape, smape, r2, mase, tu)
  )
}

#' Rolling-origin cross-validation RMSE
#' @param y_ts ts of response (log scale)
#' @param xreg matrix of exogenous regressor (aligned)
#' @param init initial training length
#' @param h horizon per fold
#' @param thr_fail failure-rate threshold for skipping fallback
#' @param stepwise pass to auto.arima
#' @param approximation pass to auto.arima
#' @return numeric RMSE
#' @export
#' @importFrom forecast auto.arima forecast Arima
#' @importFrom stats window
#' @importFrom utils flush.console
rolling_cv_rmse <- function(y_ts, xreg, init = 48, h = 12,
                            thr_fail = 0.10, stepwise = TRUE, approximation = TRUE) {
  cat("[4/6] Rolling-origin CV...\n"); utils::flush.console()
  idxs <- seq(init, length(y_ts) - h)

  fails <- sum(sapply(idxs, function(i) {
    !tryCatch({
      forecast::auto.arima(
        stats::window(y_ts, end = time(y_ts)[i]),
        xreg = xreg[1:i, , drop = FALSE],
        seasonal = TRUE, stepwise = stepwise, approximation = approximation
      )
      TRUE
    }, error = function(e) FALSE)
  }))
  use_skip <- (fails / length(idxs)) <= thr_fail
  cat(sprintf(" - Failure rate: %.1f%% -> %s\n", fails / length(idxs) * 100, if (use_skip) "SKIP" else "FALLBACK"))

  errs <- unlist(lapply(idxs, function(i) {
    y_tr <- stats::window(y_ts, end = time(y_ts)[i])
    fit <- tryCatch(
      forecast::auto.arima(y_tr, xreg = xreg[1:i, , drop = FALSE],
                           seasonal = TRUE, stepwise = stepwise, approximation = approximation),
      error = function(e) NULL
    )
    if (is.null(fit) && use_skip) return(NULL)
    if (is.null(fit)) {
      fit <- forecast::Arima(
        y_tr, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),
        xreg = xreg[1:i, , drop = FALSE], include.mean = FALSE
      )
    }
    fc <- forecast::forecast(fit, h = h, xreg = xreg[(i + 1):(i + h), , drop = FALSE])$mean
    stats::window(y_ts, start = time(y_ts)[i + 1], end = time(y_ts)[i + h]) - fc
  }))

  rmse <- sqrt(mean(errs^2, na.rm = TRUE))
  cat(sprintf(" - CV RMSE = %.3f\n\n", rmse)); utils::flush.console()
  rmse
}
