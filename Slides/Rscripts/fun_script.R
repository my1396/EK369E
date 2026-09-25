quick_summary <- function(x, full=FALSE) {
    # Function to compute basic descriptive statistics
    if (!full){
        # Basic summary
        data.frame(
          n = length(x),
          min = min(x),
          mean = mean(x),
          median = median(x),
          max = max(x),
          sd = sd(x),
          skewness = moments::skewness(x),
          kurtosis = moments::kurtosis(x),
          row.names = NULL
        )
    } else {
        # Full summary with quartiles and IQR
        data.frame(
          n = length(x),
          min = min(x),
          Q1 = quantile(x, 0.25),
          mean = mean(x),
          median = median(x),
          Q3 = quantile(x, 0.75),
          max = max(x),
          IQR = IQR(x),
          sd = sd(x),
          skewness = moments::skewness(x),
          kurtosis = moments::kurtosis(x),
          row.names = NULL
        )
    }
}

plot_png <- function(p, f_name, width, height, ppi = 300) {
  # a plot wrapper
  png(f_name, width = width * ppi, height = height * ppi, res = ppi)
  print(p)
  invisible(dev.off())
}

plot_png_base <- function(expr, f_name, width, height, ppi = 300,
                          trim = TRUE, pad = 8) {
  # For base-graphics plots (PerformanceAnalytics, plot.xts, ...), which draw
  # as a side effect rather than returning an object to print.
  #
  # trim = TRUE crops the blank border the device leaves around the plot.
  # charts.PerformanceSummary in particular reserves room for a title it is
  # not drawing, so a 9x7 canvas comes back 13% blank at the top -- which on a
  # slide reads as an unexplained gap between the figure and the text above.
  png(f_name, width = width * ppi, height = height * ppi, res = ppi)
  on.exit({
    invisible(dev.off())
    if (trim) trim_png(f_name, pad = pad)
  }, add = TRUE)
  force(expr) # the promise evaluates here, with the device open
  invisible(NULL)
}

trim_png <- function(f_name, pad = 8) {
  # Crop the uniform border off a png, then give it back `pad` pixels so the
  # ink does not touch the edge. Silently a no-op if magick is unavailable.
  if (!requireNamespace("magick", quietly = TRUE)) return(invisible(FALSE))
  img <- magick::image_read(f_name)
  img <- magick::image_trim(img)
  img <- magick::image_border(img, color = "white",
                              geometry = paste0(pad, "x", pad))
  magick::image_write(img, f_name)
  invisible(TRUE)
}


library(tidyverse)
BASE_SIZE <- 11
INK <- "#0b0b0b"
INK_SOFT <- "#52514e"
INK_MUTED <- "#8a8985"

my_theme <- theme_minimal(base_size = BASE_SIZE) +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(size = rel(1)),
    legend.text = element_text(size = rel(1)),
    legend.key.width = unit(24, "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
    panel.spacing = unit(13, "pt"),
    # facet lables if you have multiple panels
    strip.text = element_text(face = "bold", colour = INK, size = rel(1.05)),
    plot.title = element_text(face = "bold", size = rel(1.20), hjust = 0.5),
    plot.subtitle = element_text(colour = INK_SOFT, size = rel(0.95)),
    axis.title = element_text(colour = INK_SOFT, size = rel(0.95)),
    axis.text = element_text(colour = INK_SOFT, size = rel(0.90)),
    plot.caption = element_text(
      colour = INK_MUTED, hjust = 0,
      margin = margin(t = 10)
    )
  )
theme_set(my_theme)