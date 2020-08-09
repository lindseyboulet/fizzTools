#' ep_henry_sol {fizzTulz}
#' 
#' @title Calculate Henry's solubility constant
#'
#' @description Function to calculate the Henry's solubility constant for an air mixture in a bubble in fluid 
#' from the known temperature and viscosity of the system as well as the molecular radii of the bubble. 
#' Helper function for bubble lifetime model
#' 
#' @param temp_c temperature in degrees celcius 37 by default
#' @param kh Henry's Law constant (1699 for air in water) 1699 by default
#' 
#' @export

ep_henry_sol <- function(temp_c = 37, kh = 1699){0.082*(temp_c+273.15)/kh}


