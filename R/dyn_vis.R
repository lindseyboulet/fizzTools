#' dyn_vis {fizzTulz}
#' 
#' @title Calculate dynamic fluid viscosity of blood
#'
#' @description Function to calculate the dynamic fluid viscosity of blood in Pascal-second (Pa-s)
#' from the known hematocrit fraction. Helper function for bubble lifetime model
#' 
#' @param hem hematocrit fraction 0.467 by default
#' 
#' 
#' @export

dynVis <- function(hct = 0.467){(1.1*0.6)/(1-hct^(1/3))/1000}



