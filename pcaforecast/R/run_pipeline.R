# R/run_pipeline.R

#' Run full PCA-SARIMAX pipeline
#' @param file_path Excel path
#' @param pg_name product group
#' @param sheet_name sheet
#' @param forecast_h horizon
#' @param test_end c(year,month) holdout last date
#' @param sw_thr Shapiro-Wilk p-value threshold
#' @param backtest_start c(year,month)
#' @return list with model, combined, accuracy, granger, gif, gif_preview, files, backtest
#' @export
#' @importFrom utils flush.console
#' @importFrom stats fitted time shapiro.test qt residuals
#' @importFrom zoo as.yearmon
#' @importFrom forecast auto.arima forecast arimaorder
run_pipeline <- function(file_path, pg_name = "Snack",
                         sheet_name = "data", forecast_h = 12,
                         test_end = c(2024,12), sw_thr = 0.05,
                         backtest_start = c(2023,1)) {
  cat("[1/6] Loading & prep...\n"); utils::flush.console()
  p <- prep_data(file_path, sheet_name, pg_name)
  dt <- p$dt; shift <- p$shift; csi_cols <- p$csi_cols

  # zero-variance drop prior to PCA
  zv <- names(which(sapply(dt[, ..csi_cols], var) == 0))
  pca_cols <- setdiff(csi_cols, zv)
  if (length(zv)) cat("   Removed zero-variance: ", paste(zv, collapse = ", "), "\n", sep="")

  cat("[2/6] PCA...\n"); utils::flush.console()
  pca_res <- run_pca(dt, pca_cols, pg_name)
  dt <- pca_res$dt; pc_names <- pca_res$pc_names
  loadings <- pca_res$loadings; scree_file <- pca_res$scree_file; scree_df <- pca_res$scree_df

  # Granger on PCs
  cat("[3/6] Granger tests...\n"); utils::flush.console()
  gr_res <- data.frame(var=character(), lag=integer(), F=numeric(), p.value=numeric())
  best_pv <- 1; best_f <- NA; best_var <- pc_names[1]; best_lag <- 1
  for (v in pc_names) for (l in 1:12) {
    gt <- tryCatch(lmtest::grangertest(dt$Va_log ~ dt[[v]], order = l), error = function(e) NULL)
    if (!is.null(gt)) {
      pv <- gt$`Pr(>F)`[2]; fstat <- gt$F[2]
      gr_res <- rbind(gr_res, data.frame(var = v, lag = l, F = fstat, p.value = pv))
      if (!is.na(pv) && pv < best_pv) { best_pv <- pv; best_f <- fstat; best_var <- v; best_lag <- l }
    }
  }
  cat(sprintf("   Selected PC: %s | lag=%d (F=%.3f, p=%.4f)\n\n", best_var, best_lag, best_f, best_pv))

  # ts assemble
  if (!"Year" %in% names(dt) || !"Month" %in% names(dt)) {
    start_year <- 2017; start_month <- 1
  } else {
    start_year <- min(dt$Year, na.rm=TRUE)
    start_month <- min(dt$Month, na.rm=TRUE)
  }
  ex_ts <- ts(dt[[best_var]], start = c(start_year, start_month), freq = 12)
  y_ts  <- ts(dt$Va_log,     start = c(start_year, start_month), freq = 12)
  tr_len <- length(y_ts)

  # SARIMAX
  cat("[4/6] SARIMAX fit & forecast...\n"); utils::flush.console()
  xmain <- build_xreg_main(y_ts, ex_ts, L = best_lag, h_out = forecast_h)
  model <- forecast::auto.arima(y_ts, xreg = xmain$x_tr, seasonal = TRUE,
                                stepwise = FALSE, approximation = FALSE)
  fcast <- forecast::forecast(model, h = forecast_h, xreg = xmain$x_te)

  # combine
  d_act  <- seq(as.Date(sprintf("%04d-%02d-01", start_year, start_month)),
                by = "month", length.out = tr_len)
  d_pred <- seq(d_act[tr_len], by = "month", length.out = forecast_h + 1)[-1]
  combined_all <- data.frame(
    date  = c(d_act, d_pred),
    value = c(invlog(y_ts, shift), invlog(fcast$mean, shift)),
    type  = rep(c("Actual","Forecast"), c(tr_len, forecast_h))
  )

  # backtest
  cat("[5/6] Backtest & animation...\n"); utils::flush.console()
  bt_df <- run_backtest_from(
    y_ts = y_ts, ex_ts = ex_ts, shift = shift,
    start_year = backtest_start[1], start_month = backtest_start[2],
    L = best_lag
  )
  bt_plot_file <- paste0("backtest_", pg_name, "_from_", backtest_start[1],
                         sprintf("%02d", backtest_start[2]), ".png")
  plot_backtest(bt_df, pg_name, bt_plot_file)

  bt_start_date <- as.Date(sprintf("%04d-%02d-01", backtest_start[1], backtest_start[2]))
  anim <- make_forecast_gif(combined_all, pg_name, bt_df = bt_df, backtest_start_date = bt_start_date)
  gif_file <- anim$gif; gif_preview <- anim$preview

  # diagnostics
  diag_file <- paste0("diagnostics_", pg_name, ".png")
  plot_diagnostics(model, fcast, combined_all, diag_file)

  # residual normality -> t-innovations
  cat("[6/6] Residual normality...\n"); utils::flush.console()
  sw <- stats::shapiro.test(stats::residuals(model))
  if (sw$p.value < sw_thr) {
    ar_ord <- forecast::arimaorder(model)
    spec <- rugarch::ugarchspec(
      mean.model = list(armaOrder = c(ar_ord['p'], ar_ord['q']), include.mean = TRUE),
      variance.model = list(model = "sGARCH", garchOrder = c(0,0)),
      distribution.model = "std"
    )
    fit_t <- rugarch::ugarchfit(spec, data = y_ts, solver = "hybrid")
    fc_t  <- rugarch::ugarchforecast(fit_t, n.ahead = forecast_h)
    ft <- fc_t@forecast$seriesFor[,1]; st <- fc_t@forecast$sigmaFor[,1]
    df_t <- fit_t@fit$coef['shape']
    lo <- ft + stats::qt(0.025, df = df_t)*st; hi <- ft + stats::qt(0.975, df = df_t)*st
    dfp <- data.frame(date = d_pred, mean = ft, lo95 = lo, hi95 = hi)
    p_t <- ggplot2::ggplot(dfp, ggplot2::aes(date, mean)) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo95, ymax = hi95), alpha = 0.2) +
      ggplot2::labs(title = "Forecast with t-innovations (95% PI)") +
      ggplot2::theme_minimal()
    t_file <- paste0("forecast_t_", pg_name, ".png")
    ggplot2::ggsave(t_file, p_t, width = 7, height = 4, dpi = 300)
  }

  # holdout accuracy & CV
  idx <- stats::time(y_ts) <= zoo::as.yearmon(sprintf("%d-%02d", test_end[1], test_end[2])) &
    stats::time(y_ts) >  zoo::as.yearmon("2022-12")
  acc <- accuracy_tbl(invlog(y_ts[idx], shift),
                      invlog(as.numeric(stats::fitted(model))[idx], shift))
  cv  <- rolling_cv_rmse(y_ts, xreg = xmain$x_tr)
  acc <- rbind(acc, data.frame(Metric = "CV_RMSE12", Value = cv))

  # Excel export
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Prediction");   openxlsx::writeData(wb, "Prediction", combined_all)
  openxlsx::addWorksheet(wb, "Accuracy");     openxlsx::writeData(wb, "Accuracy", acc)
  openxlsx::addWorksheet(wb, "Granger");      openxlsx::writeData(wb, "Granger", gr_res)
  openxlsx::addWorksheet(wb, "PCA Loadings"); openxlsx::writeData(wb, "PCA Loadings", loadings)
  openxlsx::addWorksheet(wb, "PCA Scree");    openxlsx::writeData(wb, "PCA Scree", scree_df)
  openxlsx::addWorksheet(wb, "Scree Plot")
  openxlsx::insertImage(wb, "Scree Plot", file = scree_file, startRow = 2, startCol = 2, width = 6, height = 3)
  openxlsx::addWorksheet(wb, "Diagnostics")
  openxlsx::insertImage(wb, "Diagnostics", file = diag_file, startRow = 2, startCol = 2, width = 6, height = 10)

  openxlsx::addWorksheet(wb, "Backtest_2023on")
  if (!is.null(bt_df)) openxlsx::writeData(wb, "Backtest_2023on", bt_df, startRow = 1, startCol = 1)
  if (file.exists(bt_plot_file)) {
    start_row_img <- if (!is.null(bt_df)) 2 + nrow(bt_df) + 2 else 2
    openxlsx::insertImage(wb, "Backtest_2023on", file = bt_plot_file, startRow = start_row_img, startCol = 2, width = 8, height = 4.5)
  }

  openxlsx::addWorksheet(wb, "Animation")
  if (file.exists(gif_preview)) {
    openxlsx::insertImage(wb, "Animation", file = gif_preview, startRow = 2, startCol = 2, width = 9, height = 5)
  }
  if (!is.na(gif_file) && file.exists(gif_file)) {
    link_path <- normalizePath(gif_file, winslash = "/", mustWork = FALSE)
    openxlsx::writeData(wb, "Animation", data.frame(GIF_Link = "Open animation (GIF)"), startRow = 1, startCol = 2)
    openxlsx::writeFormula(wb, "Animation",
                           x = sprintf('HYPERLINK("%s","Open animation (GIF)")', link_path),
                           startRow = 1, startCol = 3
    )
  }

  out_file <- paste0("Predicted_", pg_name, "_PCA.xlsx")
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

  invisible(list(
    model        = model,
    combined     = combined_all,
    accuracy     = acc,
    granger      = gr_res,
    gif          = gif_file,
    gif_preview  = gif_preview,
    files        = list(
      scree        = scree_file,
      diagnostics  = diag_file,
      backtest_png = bt_plot_file,
      excel        = out_file
    ),
    backtest     = bt_df
  ))
}
