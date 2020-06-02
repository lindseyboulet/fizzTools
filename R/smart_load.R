#' smart_load {fizzTulz}
#' 
#' @title Load and Install Packages
#'
#' @description Checks for installed packages, installs and loads them
#'
#' @param pkgs Vector of r package names in string form
#'
#'
#' @examples
#' physTools::smartLoad("ggplot2")
#' physTools::smartLoad(c("ggplot2", "lme4"))
#'
#' @export


smart_load <- function(pkgs){
  for(i in 1:length(pkgs)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
  if(!is_installed(pkgs[i])){install.packages(pkgs[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
  library(pkgs[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}
}


