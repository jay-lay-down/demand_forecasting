#' Create animation (Actual + Backtest Forecast + Future Forecast)
#' @param combined data.frame(date,value,type)
#' @param pg_name label
#' @param bt_df optional backtest df(date,Actual,Forecast)
#' @param backtest_start_date Date
#' @param duration seconds
#' @param fps frames per second
#' @param width px
#' @param height px
#' @return list(gif, preview)
#' @export
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_colour_manual labs theme_minimal theme element_text geom_vline ggsave
make_forecast_gif <- function(combined, pg_name, bt_df = NULL,
                              backtest_start_date = as.Date("2023-01-01"),
                              duration = 8, fps = 20, width = 900, height = 500) {
  combined <- combined[order(combined$date), ]
  d_all <- sort(unique(c(
    combined$date,
    if (!is.null(bt_df)) bt_df$date else as.Date(character(0))
  )))

  target_frames <- max(10, duration * fps)
  n_frames <- min(length(d_all), target_frames)
  idx_seq  <- unique(as.integer(round(seq(1, length(d_all), length.out = n_frames))))
  idx_seq[idx_seq < 1] <- 1
  idx_seq[idx_seq > length(d_all)] <- length(d_all)

  df_act    <- subset(combined, type == "Actual")
  df_future <- subset(combined, type == "Forecast")
  df_bt_for <- if (!is.null(bt_df))
    data.frame(date = bt_df$date, value = bt_df$Forecast, type = "Backtest Forecast") else NULL

  y_min <- min(c(df_act$value,
                 if(nrow(df_future)) df_future$value else Inf,
                 if(!is.null(df_bt_for)) df_bt_for$value else Inf), na.rm = TRUE)
  y_max <- max(c(df_act$value,
                 if(nrow(df_future)) df_future$value else -Inf,
                 if(!is.null(df_bt_for)) df_bt_for$value else -Inf), na.rm = TRUE)

  tmp_dir <- file.path(tempdir(), paste0("gif_frames_", pg_name, "_", as.integer(Sys.time())))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  png_files <- character(0)

  for (i in idx_seq) {
    d_cut <- d_all[i]
    dat_act_cut <- subset(df_act, date <= d_cut)
    dat_fut_cut <- subset(df_future, date <= d_cut)
    if (!is.null(df_bt_for)) dat_bt_cut <- subset(df_bt_for, date <= d_cut) else dat_bt_cut <- NULL

    p <- ggplot2::ggplot() +
      (if (nrow(dat_act_cut) > 1)
        ggplot2::geom_line(data = dat_act_cut, ggplot2::aes(date, value, colour = "Actual"), linewidth = 1.1)) +
      (if (!is.null(dat_bt_cut) && nrow(dat_bt_cut) > 1)
        ggplot2::geom_line(data = dat_bt_cut, ggplot2::aes(date, value, colour = "Backtest Forecast"),
                           linewidth = 1.1, linetype = 2)) +
      (if (nrow(dat_fut_cut) > 1)
        ggplot2::geom_line(data = dat_fut_cut, ggplot2::aes(date, value, colour = "Future Forecast"),
                           linewidth = 1.1)) +
      ggplot2::scale_y_continuous(limits = c(y_min, y_max)) +
      ggplot2::scale_colour_manual(
        values = c("Actual" = "#1f77b4", "Backtest Forecast" = "#ff7f0e", "Future Forecast" = "#2ca02c"),
        breaks = c("Actual","Backtest Forecast","Future Forecast")
      ) +
      ggplot2::labs(
        title = paste0("Actual, Backtest Forecast (from ", format(backtest_start_date, "%Y-%m"), "), and Future Forecast - ", pg_name),
        subtitle = format(d_cut, "%Y-%m"),
        x = "Date", y = "Value", colour = NULL
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "top", plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::geom_vline(xintercept = as.numeric(backtest_start_date), linetype = "dashed", colour = "grey40")

    fpath <- file.path(tmp_dir, sprintf("frame_%04d.png", i))
    ggplot2::ggsave(fpath, p, width = width/150, height = height/150, dpi = 150)
    png_files <- c(png_files, fpath)
  }

  gif_file <- sprintf("anim_%s.gif", pg_name)
  png_preview <- sprintf("anim_%s_preview.png", pg_name)

  backend <- get0("GIF_BACKEND", ifnotfound = "none")
  if (identical(backend, "gifski") && requireNamespace("gifski", quietly = TRUE)) {
    gifski::gifski(png_files, gif_file, width = width, height = height, delay = 1/fps, loop = TRUE)
  } else if (identical(backend, "magick") && requireNamespace("magick", quietly = TRUE)) {
    imgs <- magick::image_read(png_files)
    anim <- magick::image_animate(imgs, fps = fps, loop = 0)
    magick::image_write(anim, gif_file)
  } else {
    message("GIF backend not available; only preview PNG will be created. (Install 'gifski' or 'magick')")
    gif_file <- NA_character_
  }

  try({
    if (!is.na(gif_file) && file.exists(gif_file) && requireNamespace("magick", quietly = TRUE)) {
      img <- magick::image_read(gif_file)
      magick::image_write(img[1], path = png_preview, format = "png")
    } else if (length(png_files) > 0) {
      file.copy(png_files[1], png_preview, overwrite = TRUE)
    }
  }, silent = TRUE)

  unlink(tmp_dir, recursive = TRUE, force = TRUE)
  list(gif = gif_file, preview = png_preview)
}
