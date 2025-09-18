# R/utils.R
# Small helpers: inverse log, data prep.

#' Inverse of log with positive shift
#' @param z numeric vector (log scale)
#' @param shift positive shift used in log(x + shift)
#' @return numeric vector on original scale
#' @export
invlog <- function(z, shift) exp(z) - shift

#' Read and preprocess wide Excel sheet
#'
#' Expects columns: PG, Va, Year, Month, and CSI1:CSI85.
#' Applies log(x + shift) to Va and CSI columns.
#'
#' @param path Excel file path
#' @param sheet sheet name containing data (default "data")
#' @param pg product group filter value (default "Snack")
#' @return list(dt=data.table, shift=numeric, csi_cols=character)
#' @export
#' @importFrom readxl read_excel
#' @importFrom data.table as.data.table
#' @importFrom stats var
prep_data <- function(path, sheet = "data", pg = "Snack") {
  dt <- readxl::read_excel(path, sheet = sheet)
  dt <- data.table::as.data.table(dt)

  if (!"PG" %in% names(dt)) stop("Column 'PG' not found.")
  if (!"Va" %in% names(dt)) stop("Column 'Va' not found (target).")

  dt <- dt[PG == pg]

  shift <- abs(min(dt$Va, na.rm = TRUE)) + 1
  dt[, Va_log := log(Va + shift)]

  csi_cols <- sprintf("CSI%d", 1:85)
  miss <- setdiff(csi_cols, names(dt))
  if (length(miss)) stop("Missing CSI columns: ", paste(miss, collapse = ", "))

  dt[, (csi_cols) := lapply(.SD, function(x) log(x + shift)), .SDcols = csi_cols]

  list(dt = dt, shift = shift, csi_cols = csi_cols)
}
