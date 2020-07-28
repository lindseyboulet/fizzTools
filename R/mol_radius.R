#' mol_radius {fizzTulz}
#' 
#' @title Calculate the molecular radius of a bubble
#'
#' @description Function to calculate the molecular radius of an air bubble suspended in fluid 
#' from the known fraction of O2, N2 and CO2 within. Uses known molecular radii values of O2, N2, CO2 
#' Helper function for bubble lifetime model
#' 
#' @param fiO2 fraction of O2 in bubble 0.21 by default
#' @param fiN2 fraction of N2 in bubble 0.7896 by default
#' @param fiCO2 fraction of CO2 in bubble 0.0004 by default
#' 
#' 
#' 
#' @export

molRad <- function(fiO2 = 0.21,
                   fiN2 = 0.7896,
                   fiCO2 = 0.0004){
  (fiO2*103E-12) + (fiN2*116E-12) + (fiCO2*113E-12)
}


