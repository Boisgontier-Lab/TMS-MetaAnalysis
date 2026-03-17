#' Create forest plot
#' 
#' Generates a RevMan-style forest plot for meta-analysis results.
#' 
#' @param meta_model metacor object from sensitivity meta-analysis
#' @param xlim X-axis limits (default: c(-0.9, 0.9))
#' @param prediction Show prediction interval (default: TRUE)
#' @param fs_hetstat Font size for heterogeneity statistics (default: 10)
#' @param addrows_below Number of rows below overall (default: 2)
#' @return Forest plot (displayed)
create_forest_plot <- function(meta_model,
                                xlim = c(-0.9, 0.9),
                                prediction = TRUE,
                                fs_hetstat = 10,
                                addrows_below = 2) {
  forest(
    meta_model,
    layout = "Revman5",
    common = FALSE,
    xlim = xlim,
    prediction = prediction,
    fs.hetstat = fs_hetstat,
    col.subgroup = "black",
    addrows.below.overall = addrows_below
  )
}

#' Save forest plot to SVG
#' 
#' Exports a forest plot to SVG format.
#' 
#' @param meta_model metacor object from sensitivity meta-analysis
#' @param filename Output filename (without extension)
#' @param height Plot height in inches (default: 7)
#' @param width Plot width in inches (default: 7)
#' @param output_dir Output directory (default: ".")
#' @param xlim X-axis limits (default: c(-0.9, 0.9))
#' @return NULL (saves file as side effect)
save_forest_plot_svg <- function(meta_model,
                                  filename,
                                  height = 7,
                                  width = 7,
                                  output_dir = ".",
                                  xlim = c(-0.9, 0.9)) {
  filepath <- file.path(output_dir, paste0(filename, ".svg"))
  
  svglite(filepath, height = height, width = width)
  
  forest(
    meta_model,
    layout = "Revman5",
    common = FALSE,
    xlim = xlim,
    prediction = TRUE,
    fs.hetstat = 10,
    col.subgroup = "black",
    addrows.below.overall = 2
  )
  
  dev.off()
  
  message(paste("Forest plot saved to:", filepath))
}

#' Create contour-enhanced funnel plot
#' 
#' Generates a funnel plot with contour lines for significance levels.
#' 
#' @param meta_model metacor object from sensitivity meta-analysis
#' @param title Plot title
#' @param xlim X-axis limits (default: c(-1.5, 1))
#' @param contour_levels Contour levels (default: c(0.95, 0.99))
#' @param legend_x X position for legend (default: 0.5)
#' @param legend_y Y position for legend (default: 0.01)
#' @return Funnel plot (displayed)
create_funnel_plot <- function(meta_model,
                                title,
                                xlim = c(-1.5, 1),
                                contour_levels = c(0.95, 0.99),
                                legend_x = 0.5,
                                legend_y = 0.01) {
  col_contour <- c("gray85", "gray75")
  
  meta::funnel(
    meta_model,
    xlim = xlim,
    contour = contour_levels,
    col.contour = col_contour
  )
  
  legend(
    x = legend_x,
    y = legend_y,
    legend = c("p < 0.05", "p < 0.01"),
    fill = col_contour
  )
  
  title(title)
}

#' Save funnel plot to PNG
#' 
#' Exports a contour-enhanced funnel plot to PNG format.
#' 
#' @param meta_model metacor object from sensitivity meta-analysis
#' @param filename Output filename (without extension)
#' @param title Plot title
#' @param width Plot width in pixels (default: 2500)
#' @param height Plot height in pixels (default: 2000)
#' @param res Resolution in DPI (default: 300)
#' @param output_dir Output directory (default: ".")
#' @return NULL (saves file as side effect)
save_funnel_plot_png <- function(meta_model,
                                  filename,
                                  title,
                                  width = 2500,
                                  height = 2000,
                                  res = 300,
                                  output_dir = ".") {
  filepath <- file.path(output_dir, paste0(filename, ".png"))
  
  png(file = filepath, width = width, height = height, res = res)
  
  create_funnel_plot(meta_model, title)
  
  dev.off()
  
  message(paste("Funnel plot saved to:", filepath))
}
