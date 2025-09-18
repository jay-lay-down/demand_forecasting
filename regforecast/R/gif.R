# R/gif.R
# Create an animated GIF: Actual + Backtest Forecast + Future Forecast

#' Create animation (GIF) of Actual / Backtest Forecast / Future Forecast
#' @param combined data.frame: columns date<Date>, value<numeric>, type {"Actual","Forecast"}
#' @param pg_name character label for title/filename
#' @param bt_df optional data.frame: date<Date>, Actual, Forecast (from backtest)
#' @param backtest_start_date Date for dashed vline
#' @param duration seconds
#' @param fps frames per second
#' @param width,height pixel size of GIF
#' @return list(gif=path, preview=png path)
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_colour_manual scale_y_continuous scale_x_date labs theme_minimal theme element_text ggsave geom_vline
make_forecast_gif <- function(combined, pg_name, bt_df = NULL,
                              backtest_start_date = as.Date("2023-01-01"),
                              duration = 8, fps = 20, width = 900, height = 500) {

  # ---- 0) Type/date normalization ----
  stopifnot(inherits(combined$date, "Date"))
  if (!is.null(bt_df)) stopifnot(inherits(bt_df$date, "Date"))

  # Standard legend levels
  levels_vec <- c("Actual", "Backtest Forecast", "Future Forecast")

  # Normalize names: "Forecast" -> "Future Forecast"
  combined$type[combined$type == "Forecast"] <- "Future Forecast"
  combined$type <- factor(combined$type, levels = levels_vec)

  df_bt_for <- NULL
  if (!is.null(bt_df)) {
    df_bt_for <- data.frame(
      date  = bt_df$date,
      value = bt_df$Forecast,
      type  = factor("Backtest Forecast", levels = levels_vec)
    )
  }

  # ---- 1) Frame timetable ----
  combined <- combined[order(combined$date), ]
  d_all <- sort(unique(c(combined$date, if (!is.null(df_bt_for)) df_bt_for$date)))
  target_frames <- max(10, duration * fps)
  n_frames <- min(length(d_all), target_frames)
  idx_seq <- unique(as.integer(round(seq(1, length(d_all), length.out = n_frames))))
  idx_seq[idx_seq < 1] <- 1
  idx_seq[idx_seq > length(d_all)] <- length(d_all)

  # Split data
  df_act    <- subset(combined, type == "Actual")
  df_future <- subset(combined, type == "Future Forecast")

  # y-range
  y_min <- min(c(df_act$value,
                 if (nrow(df_future)) df_future$value else Inf,
                 if (!is.null(df_bt_for)) df_bt_for$value else Inf), na.rm = TRUE)
  y_max <- max(c(df_act$value,
                 if (nrow(df_future)) df_future$value else -Inf,
                 if (!is.null(df_bt_for)) df_bt_for$value else -Inf), na.rm = TRUE)

  # Temp frames
  tmp_dir <- file.path(tempdir(), paste0("gif_frames_", pg_name, "_", as.integer(Sys.time())))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  png_files <- character(0)

  # Fixed colour scale (names must match levels exactly)
  col_vals <- c("Actual" = "#1f77b4", "Backtest Forecast" = "#ff7f0e", "Future Forecast" = "#2ca02c")

  for (i in idx_seq) {
    d_cut <- d_all[i]
    dat_act_cut    <- subset(df_act,    date <= d_cut)
    dat_future_cut <- subset(df_future, date <= d_cut)
    dat_bt_cut     <- if (!is.null(df_bt_for)) subset(df_bt_for, date <= d_cut) else NULL

    # Dummy to lock legend levels (invisible)
    dummy_levels <- data.frame(
      date  = as.Date(NA),
      value = NA_real_,
      type  = factor(levels_vec, levels = levels_vec)
    )

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = dummy_levels, ggplot2::aes(date, value, colour = type), alpha = 0) +

      if (nrow(dat_act_cut) > 0)
        ggplot2::geom_line(data = dat_act_cut, ggplot2::aes(date, value, colour = type), linewidth = 1.1) else NULL +

      if (!is.null(dat_bt_cut) && nrow(dat_bt_cut) > 0)
        ggplot2::geom_line(data = dat_bt_cut, ggplot2::aes(date, value, colour = type),
                           linewidth = 1.1, linetype = 2) else NULL +

      if (nrow(dat_future_cut) > 0)
        ggplot2::geom_line(data = dat_future_cut, ggplot2::aes(date, value, colour = type), linewidth = 1.1) else NULL +

      ggplot2::scale_y_continuous(limits = c(y_min, y_max)) +
      ggplot2::scale_colour_manual(values = col_vals,
                                   breaks = levels_vec, limits = levels_vec, drop = FALSE) +
      ggplot2::scale_x_date(date_labels = "%Y-%m") +
      ggplot2::labs(
        title    = paste0("Actual, Backtest Forecast (from ", format(as.Date(backtest_start_date), "%Y-%m"),
                          "), and Future Forecast — ", pg_name),
        subtitle = format(d_cut, "%Y-%m"),
        x = "Date", y = "Value", colour = NULL
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "top",
                     plot.title = ggplot2::element_text(face = "bold")) +
      # pass Date object (not numeric) to avoid warnings
      ggplot2::geom_vline(xintercept = as.Date(backtest_start_date), linetype = "dashed", colour = "grey40")

    fpath <- file.path(tmp_dir, sprintf("frame_%04d.png", i))
    ggplot2::ggsave(fpath, p, width = width/150, height = height/150, dpi = 150)
    png_files <- c(png_files, fpath)
  }

  gif_file <- sprintf("anim_%s.gif", pg_name)
  png_preview <- sprintf("anim_%s_preview.png", pg_name)

  backend <- if (exists("GIF_BACKEND", inherits = TRUE)) get("GIF_BACKEND", inherits = TRUE) else "none"
  if (backend == "gifski") {
    gifski::gifski(png_files, gif_file, width = width, height = height, delay = 1/fps, loop = TRUE)
  } else if (backend == "magick") {
    imgs <- magick::image_read(png_files)
    anim <- magick::image_animate(imgs, fps = fps, loop = 0)
    magick::image_write(anim, gif_file)
  } else {
    message("GIF backend not available; only preview PNG will be created. (Install 'gifski' or 'magick')")
    gif_file <- NA_character_
  }

  # Preview (first frame)
  try({
    if (!is.na(gif_file) && file.exists(gif_file) && requireNamespace("magick", quietly = TRUE)) {
      img <- magick::image_read(gif_file)
      magick::image_write(img[1], path = png_preview, format = "png")
    } else if (length(png_files) > 0) {
      file.copy(png_files[1], png_preview, overwrite = TRUE)
    }
  }, silent = TRUE)

  unlink(tmp_dir, recursive = TRUE, force = TRUE)
  message(" -> GIF saved: ", ifelse(is.na(gif_file), "<disabled>", gif_file))
  message(" -> Preview saved: ", png_preview)
  list(gif = gif_file, preview = png_preview)
}
