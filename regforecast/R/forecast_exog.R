#' Forecast exogenous series
#' @param series numeric or ts
#' @param h horizon
#' @return ts mean forecast
#' @export
#' @importFrom stats is.ts
forecast_exog <- function(series, h) {
  if (!stats::is.ts(series)) series <- stats::ts(series, freq = 12)
  forecast::forecast(
    forecast::auto.arima(series, seasonal = TRUE, stepwise = FALSE, approximation = FALSE),
    h = h
  )$mean
}
