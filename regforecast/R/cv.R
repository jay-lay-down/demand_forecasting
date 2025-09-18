#' #' Rolling-origin cross validation RMSE
#'
#' @param y_ts        Response time series (ts).
#' @param xreg        Matrix of regressors aligned with \code{y_ts}.
#' @param init        Integer, initial training length.
#' @param h           Integer, forecast horizon per fold.
#' @param thr_fail    Failure threshold (fraction) to decide SKIP vs FALLBACK.
#' @param stepwise    Passed to \code{forecast::auto.arima}.
#' @param approximation Passed to \code{forecast::auto.arima}.
#' @return Numeric RMSE over all folds.
#' @noRd
#' @export
rolling_cv_rmse <- function(y_ts, xreg, init = 48, h = 12, thr_fail = 0.10, stepwise = TRUE, approximation = TRUE) {
  cat("[4/6] Rolling-origin CV...\n"); flush.console()
  idxs <- seq(init, length(y_ts) - h)
  fails <- sum(sapply(idxs, function(i) {
    !tryCatch({
      forecast::auto.arima(
        stats::window(y_ts, end = stats::time(y_ts)[i]),
        xreg = xreg[1:i,, drop = FALSE],
        seasonal = TRUE, stepwise = stepwise, approximation = approximation
      ); TRUE
    }, error = function(e) FALSE)
  }))
  use_skip <- (fails / length(idxs)) <= thr_fail
  cat(sprintf("Failure rate: %.1f%% -> %s\n", fails/length(idxs)*100, if (use_skip) "SKIP" else "FALLBACK"))

  errs <- unlist(lapply(idxs, function(i) {
    y_tr <- stats::window(y_ts, end = stats::time(y_ts)[i])
    fit <- tryCatch(
      forecast::auto.arima(y_tr, xreg = xreg[1:i,, drop = FALSE], seasonal = TRUE, stepwise = stepwise, approximation = approximation),
      error = function(e) NULL
    )
    if (is.null(fit) && use_skip) return(NULL)
    if (is.null(fit)) {
      fit <- forecast::Arima(
        y_tr, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12),
        xreg = xreg[1:i,, drop = FALSE], include.constant = FALSE
      )
    }
    fc <- forecast::forecast(fit, h = h, xreg = xreg[(i + 1):(i + h),, drop = FALSE])$mean
    stats::window(y_ts, start = stats::time(y_ts)[i + 1], end = stats::time(y_ts)[i + h]) - fc
  }))
  rmse <- sqrt(mean(errs^2, na.rm = TRUE))
  cat(sprintf("CV RMSE = %.3f\n\n", rmse)); flush.console()
  rmse
}
