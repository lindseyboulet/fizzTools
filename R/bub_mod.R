#' bub_mod {fizzTulz}
#' 
#' @title Two compartment model to calculate time to bubble dissolution
#'
#' @description Function to model the time to dissolution of a bubble suspended in a flowing solution using 
#' the initial bubble radius, barometric and fluid pressure and the diffusivity and Henry's solubility 
#' constant of the gas in the bubble. 
#' 
#' @param Ro initial bubble radius in μm 10 by default
#' @param Pb barometric pressure in mmHg 760 by default
#' @param Pf pressure of fluid in mmHg (Healthy PASP = 17 mmHg) 17 by default
#' @param D diffusivity of gas mixture in bubble (see fizzTulz::diffusivity())
#' @param s Henry's Solubility coefficient for air in water (see fizzTulz::henry_sol)
#' 
#' @export

bub_mod <- function(Ro = 10, Pb = 760, Pf, D, s){
  R <- 0
  P <- (Pb +Pf)/Pb
  (-(R^2-Ro^2)/(2*D*s*(1-0.9)))+((2*(2*0.9+1)*0.5*(R-Ro))/(3*D*s*(1-0.9)^2*P))-(((4*(2*0.9+1)*0.5^2)/(3*D*s*(1-0.9)^3*P^2))*log(((1-0.9)*P*R+2*0.5)/((1-0.9)*P*Ro+2*0.5)))
}


