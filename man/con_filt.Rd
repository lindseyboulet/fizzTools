% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/con_filt.R
\name{con_filt}
\alias{con_filt}
\title{Returns meaningful contrasts from emmeans output}
\usage{
con_filt(e1, alpha = 0.05)
}
\arguments{
\item{e1}{emmeans output object}

\item{alpha}{critical p-value to filter contrasts}
}
\description{
Function to filter contrasts from emmeans output based on p-value and that only have on factor difference
}
\details{
con_filt {fizzTulz}
}
