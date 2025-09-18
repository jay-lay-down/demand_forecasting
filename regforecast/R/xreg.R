# R/xreg.R
# Exogenous forecasting and xreg builders.

#' Forecast an exogenous series with ARIMA
#' @param series numeric or ts
#' @param h integer forecast horizon
#' @return ts mean forecast
#' @export
#' @importFrom stats is.ts
#' @importFrom forecast auto.arima forecast
forecast_exog <- function(series, h) {
  if (!stats::is.ts(series)) series <- ts(series, freq = 12)
  forecast::forecast(
    forecast::auto.arima(series, seasonal = TRUE, stepwise = FALSE, approximation = FALSE),
    h = h
  )$mean
}

#' Build main xreg for forward forecasting (y_t ~ x_{t-L})
#'
#' @param y_ts response ts
#' @param ex_ts exogenous ts (observed history)
#' @param L lag to align x_{t-L}
#' @param h_out forecast horizon
#' @return list(x_tr, x_te) matrices
#' @export
#' @importFrom data.table shift
build_xreg_main <- function(y_ts, ex_ts, L, h_out) {
  Tlen <- length(y_ts)
  ex_future <- forecast_exog(ex_ts, h_out)
  ex_raw <- c(as.numeric(ex_ts), as.numeric(ex_future))
  x_lag <- data.table::shift(ex_raw, n = L, type = "lag")
  x_lag[is.na(x_lag)] <- x_lag[which(!is.na(x_lag))[1]]  # front fill

  list(
    x_tr = matrix(x_lag[1:Tlen], ncol = 1),
    x_te = matrix(x_lag[(Tlen + 1):(Tlen + h_out)], ncol = 1)
  )
}

#' Build backtest xreg using only training data to forecast exog
#'
#' @param y_ts response ts
#' @param ex_ts exogenous ts
#' @param L lag
#' @param train_end c(year, month) end point of training
#' @return list(x_tr, x_te, h_bt, Ttr)
#' @export
#' @importFrom stats window
build_xreg_backtest <- function(y_ts, ex_ts, L, train_end) {
  y_tr <- stats::window(y_ts, end = train_end)
  Ttr  <- length(y_tr)
  h_bt <- length(y_ts) - Ttr
  if (h_bt <= 0) return(NULL)

  ex_tr <- stats::window(ex_ts, end = train_end)
  ex_future <- forecast_exog(ex_tr, h_bt)
  ex_raw <- c(as.numeric(ex_tr), as.numeric(ex_future))
  x_lag  <- data.table::shift(ex_raw, n = L, type = "lag")
  x_lag[is.na(x_lag)] <- x_lag[which(!is.na(x_lag))[1]]

  list(
    x_tr = matrix(x_lag[1:Ttr], ncol = 1),
    x_te = matrix(x_lag[(Ttr + 1):(Ttr + h_bt)], ncol = 1),
    h_bt = h_bt, Ttr = Ttr
  )
}
