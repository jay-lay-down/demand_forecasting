
# R/metrics.R
# Accuracy metrics

#' Accuracy table (MAE, RMSE, MAPE, sMAPE, MASE, Theil_U, R2)
#'
#' @param act numeric actual values
#' @param pred numeric predicted values
#' @return data.frame with metrics
#' @export
#' @importFrom stats cor
accuracy_tbl <- function(act, pred) {
  mae   <- mean(abs(act - pred))
  rmse  <- sqrt(mean((act - pred)^2))
  mape  <- mean(abs((act - pred) / act)) * 100
  smape <- mean(2 * abs(pred - act) / (abs(pred) + abs(act))) * 100

  # scale for MASE uses diff=1 by default here
  sc <- mean(abs(base::diff(act)), na.rm = TRUE)
  mase <- if (is.finite(sc) && sc > 0) mae / sc else NA_real_

  # Theil's U (one symmetric form)
  den <- sum(base::diff(act)^2, na.rm = TRUE)
  tu  <- if (den > 0) sqrt(sum((pred - act)^2, na.rm = TRUE) / den) else NA_real_

  r2 <- suppressWarnings(stats::cor(act, pred, use = "complete.obs")^2)

  data.frame(
    Metric = c("MAE","RMSE","MAPE","sMAPE","MASE","Theil_U","R2"),
    Value  = c(mae, rmse, mape, smape, mase, tu, r2)
  )
}

