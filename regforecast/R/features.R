# R/features.R
# Elastic-Net feature selection (glmnet)

#' Elastic-Net feature selection
#'
#' @param dt data.table with response and candidate features
#' @param y  character name of response column (e.g., "Va_log")
#' @param xs character vector of candidate feature names (e.g., CSI1:CSI85)
#' @param alpha mixing parameter in Elastic-Net (0=LASSO, 1=Ridge)
#' @param k number of CV folds
#' @return character vector of selected feature names
#' @export
#' @importFrom glmnet cv.glmnet
#' @importFrom stats coef var
#' @importFrom utils flush.console
select_features <- function(dt, y, xs, alpha = 0.5, k = 10) {
  cat("[3/6] Elastic-Net feature selection...\n"); utils::flush.console()

  x_mat <- as.matrix(dt[, ..xs])
  y_vec <- dt[[y]]

  # Guard: drop NA-only or zero-variance columns
  na_cols  <- which(colSums(!is.na(x_mat)) <= 1)
  zv_cols  <- which(apply(x_mat, 2, function(col) isTRUE(all.equal(stats::var(col, na.rm = TRUE), 0))))
  drop_idx <- sort(unique(c(na_cols, zv_cols)))
  if (length(drop_idx)) {
    keep <- setdiff(seq_len(ncol(x_mat)), drop_idx)
    x_mat <- x_mat[, keep, drop = FALSE]
    xs    <- xs[keep]
    cat(" - Dropped ", length(drop_idx), " NA-only/zero-variance columns\n", sep = "")
  }
  if (ncol(x_mat) == 0L) stop("No usable predictors after filtering.")

  fit <- glmnet::cv.glmnet(x_mat, y_vec, alpha = alpha, family = "gaussian", nfolds = k)

  # Use stats::coef generic to avoid roxygen import NOTE
  cf <- stats::coef(fit, s = "lambda.min")
  sel_idx <- which(cf[-1, 1] != 0)
  sel <- xs[sel_idx]

  if (length(sel) == 0L) {
    vrs <- apply(x_mat, 2, stats::var, na.rm = TRUE)
    ord <- order(vrs, decreasing = TRUE)
    take <- seq_len(min(5, length(ord)))
    sel <- xs[ord[take]]
    cat("No ENet features; fallback to top-variance: ",
        paste(sel, collapse = ", "), "\n", sep = "")
  } else {
    cat(sprintf("Selected %d features\n", length(sel)))
  }
  cat("\n")
  sel
}
