% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/align_save.R
\name{align_save}
\alias{align_save}
\title{Align Plots in a Grid and Save to PDF}
\usage{
align_save(plots.list, ncol, fileName, width = 8.5, height = 11)
}
\arguments{
\item{plots.list}{List of ggplot objects}

\item{ncol}{Number of columns in grid}

\item{fileName}{Output filename and extension (.pdf)}

\item{width}{Width of output pdf (in) 8.5 by default}

\item{height}{Width of output pdf (in) 11 by default}
}
\value{
Saves a pdf file in current directory
}
\description{
Aligns grid of plots by axes and saves as pdf
}
\details{
align_save {fizzTulz}
}
\examples{
p1 <- ggplot2::ggplot(mtcars) +ggplot2::geom_point(ggplot2::aes(wt, mpg))
p2 <- ggplot2::ggplot(mtcars) +ggplot2::geom_point(ggplot2::aes(wt, cyl))
physTools::alignSave(list(p1,p2), 1, "plot1.pdf")

}
