% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ep_bub_mod.R
\name{ep_bub_mod}
\alias{ep_bub_mod}
\title{Two compartment model to calculate time to bubble dissolution}
\usage{
ep_bub_mod(Ro = 10, Pb = 760, Pf, D, s)
}
\arguments{
\item{Ro}{initial bubble radius in μm 10 by default}

\item{Pb}{barometric pressure in mmHg 760 by default}

\item{Pf}{pressure of fluid in mmHg (Healthy PASP = 17 mmHg) 17 by default}

\item{D}{diffusivity of gas mixture in bubble (see fizzTulz::diffusivity())}

\item{s}{Henry's Solubility coefficient for air in water (see fizzTulz::henry_sol)}
}
\description{
Function to model the time to dissolution of a bubble suspended in a flowing solution using 
the initial bubble radius, barometric and fluid pressure and the diffusivity and Henry's solubility 
constant of the gas in the bubble.
}
\details{
ep_bub_mod {fizzTulz}
}
