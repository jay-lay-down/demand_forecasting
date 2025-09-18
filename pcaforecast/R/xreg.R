#' Forecast exogenous series with ARIMA
#' @param series numeric or ts
#' @param h horizon
#' @return ts
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

#' Build main xreg (y_t ~ x_{t-L}) for forward forecasting
#' @param y_ts ts
#' @param ex_ts ts
#' @param L lag
#' @param h_out horizon
#' @return list(x_tr, x_te)
#' @export
#' @importFrom data.table shift
build_xreg_main <- function(y_ts, ex_ts, L, h_out) {
  Tlen <- length(y_ts)
  ex_future <- forecast_exog(ex_ts, h_out)
  ex_raw <- c(as.numeric(ex_ts), as.numeric(ex_future))
  x_lag <- data.table::shift(ex_raw, n = L, type = "lag")
  # front fill
  if (all(is.na(x_lag))) stop("x_lag is all NA")
  x_lag[is.na(x_lag)] <- x_lag[which(!is.na(x_lag))[1]]

  list(
    x_tr = matrix(x_lag[1:Tlen], ncol = 1),
    x_te = matrix(x_lag[(Tlen + 1):(Tlen + h_out)], ncol = 1)
  )
}

#' Build backtest xreg (train-only exog forecast)
#' @param y_ts ts
#' @param ex_ts ts
#' @param L lag
#' @param train_end c(year, month)
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
  if (all(is.na(x_lag))) stop("x_lag is all NA (backtest)")
  x_lag[is.na(x_lag)] <- x_lag[which(!is.na(x_lag))[1]]

  list(
    x_tr = matrix(x_lag[1:Ttr], ncol = 1),
    x_te = matrix(x_lag[(Ttr + 1):(Ttr + h_bt)], ncol = 1),
    h_bt = h_bt, Ttr = Ttr
  )
}
