#' sev_trans {fizzTulz}
#' 
#' @title Severinghaus Calculation for Hb/-O2 Dissociation  
#'
#' @description Transforms PO2 values to SpO2 values and vice versa
#' 
#' @param po2 Partial pressure of arterial blood oxygen (mmHg)
#' @param spo2 Peripheral oxygen saturation (fraction or %)
#'
#' @examples
#' sevTrans(po2 = 55)
#' sevTrans(spo2 = 75)
#' sevTrans(spo2 = 0.75)
#' 
#' @references
#' Severinghaus JW. Simple, accurate equations for human blood O2 dissociation computations. J Appl Physiol Respir Environ Exerc Physiol. 1979;46(3):599‐602. doi:10.1152/jappl.1979.46.3.599
#'
#' @export

sev_trans <- function(po2 = NA, spo2 = NA){
  if(!is.na(po2)){
    ((((((po2^3)+(150*po2))^-1)*23400)+1)^-1)*100
  } else if(!is.na(spo2)){
    if(spo2>1){spo2<-spo2/100}
    exp((log(((spo2^-1)-1)^-1)*0.385) + 3.32 - ((spo2*72)^-1) - ((spo2^6)/6))
  }else{
    NA
  }
}

