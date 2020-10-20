#' align_save {fizzTulz}
#' 
#' @title Align Plots in a Grid and Save to PDF
#'
#' @description Aligns grid of plots by axes and saves as pdf
#'
#' @param plots.list List of ggplot objects
#' @param ncol Number of columns in grid
#' @param fileName Output filename and extension (.pdf)
#' @param width Width of output pdf (in) 8.5 by default
#' @param height Width of output pdf (in) 11 by default
#'
#' @return Saves a pdf file in current directory
#'
#' @importFrom Cairo CairoPDF
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid unit.pmax
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' p1 <- ggplot2::ggplot(mtcars) +ggplot2::geom_point(ggplot2::aes(wt, mpg))
#' p2 <- ggplot2::ggplot(mtcars) +ggplot2::geom_point(ggplot2::aes(wt, cyl))
#' physTools::alignSave(list(p1,p2), 1, "plot1.pdf")
#'
#' @export


align_save <- function(plots.list, ncol, fileName, width = 8.5, height = 11){

  grobs.list <- lapply(plots.list, ggplot2::ggplotGrob)

  widths.list <- do.call(grid::unit.pmax, lapply(grobs.list, "[[", 'widths'))
  grobs.list <- lapply(grobs.list, function(x) {
    x[['widths']] = widths.list
    x})

  Cairo::CairoPDF(file=fileName, width = width, height = height)
  gridExtra::grid.arrange(grobs = grobs.list, ncol = ncol)
  dev.off()
}
