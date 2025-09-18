#' Read and preprocess data
#' @param path Path to xlsx file
#' @param sheet Sheet name
#' @param pg Product group filter value
#' @return list(dt, shift, csi_cols)
#' @export
#' @importFrom readxl read_excel
#' @importFrom data.table as.data.table := .SD
prep_data <- function(path, sheet = "data", pg = "Snack") {
  cat("[2/6] Reading and preprocessing...\n"); flush.console()
  dt <- readxl::read_excel(path, sheet = sheet)
  dt <- data.table::as.data.table(dt)
  if (!"PG" %in% names(dt)) stop("Column 'PG' not found.")
  dt <- dt[PG == pg]
  if (!"Va" %in% names(dt)) stop("Column 'Va' (target) not found.")

  shift <- abs(min(dt$Va, na.rm = TRUE)) + 1
  dt[, Va_log := log(Va + shift)]

  csi_cols <- sprintf("CSI%d", 1:85)
  miss <- setdiff(csi_cols, names(dt))
  if (length(miss)) stop("Missing CSI columns: ", paste(miss, collapse = ", "))

  dt[, (csi_cols) := lapply(.SD, function(x) log(x + shift)), .SDcols = csi_cols]
  list(dt = dt, shift = shift, csi_cols = csi_cols)
}
