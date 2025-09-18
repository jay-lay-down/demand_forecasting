#' PCA on CSI1:CSI85 and scree plot
#' @param dt data.table
#' @param csi_cols character
#' @param pg_name label
#' @return list(dt, pc_names, loadings, scree_file, scree_df)
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point labs theme_minimal ggsave
run_pca <- function(dt, csi_cols, pg_name) {
  pca <- prcomp(dt[, ..csi_cols], center = TRUE, scale. = TRUE)
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  scree_df <- data.frame(
    PC       = paste0("PC", seq_along(var_exp)),
    Variance = var_exp,
    CumVar   = cumsum(var_exp)
  )
  p <- ggplot2::ggplot(scree_df, ggplot2::aes(PC, Variance)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = CumVar, group = 1)) +
    ggplot2::geom_point(ggplot2::aes(y = CumVar)) +
    ggplot2::labs(title = "Scree Plot (Variance Explained)", y = "Proportion") +
    ggplot2::theme_minimal(base_size = 12)
  scree_file <- paste0("scree_", pg_name, ".png")
  ggplot2::ggsave(scree_file, p, width = 7, height = 4, dpi = 300)

  k <- min(which(cumsum(var_exp) >= 0.80)[1], 20)
  scores <- data.table::as.data.table(pca$x[, 1:k, drop = FALSE])
  pc_names <- paste0("CSI", 85 + seq_len(k))
  data.table::setnames(scores, pc_names)
  dt <- cbind(dt, scores)

  loadings <- data.table::as.data.table(pca$rotation[, 1:k, drop = FALSE], keep.rownames = "Variable")
  data.table::setnames(loadings, c("Variable", pc_names))

  list(dt = dt, pc_names = pc_names, loadings = loadings, scree_file = scree_file, scree_df = scree_df)
}
