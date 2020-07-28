#' diffusivity {fizzTulz}
#' 
#' @title Calculate the summed diffusivity of an air bubble
#'
#' @description Function to calculate the summed diffusivity of air bubble suspended in fluid 
#' from the known temperature and viscosity of the system as well as the molecular radii of the bubble. 
#' Helper function for bubble lifetime model
#' 
#' 
#' @param mol_rad molecular radius of bubble (use mol_radius function)
#' @param temp_c temperature in degrees celcius 37 by default
#' @param vis viscosity of fluid surrounding bubble (cP) 2.94 by default
#' 
#' @export

diffusivity <- function(mol_rad, temp_c = 37, vis = 2.94){
  (1.38E-23*(temp_c + 273))/(6*pi*vis)*mol_rad*1E10
}


