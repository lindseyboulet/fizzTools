#' lvEnergy {fizztools}
#' 
#' @title Non-Invasive Left Ventricular Stroke Energy
#'
#' @description Calculates LV stroke energy (kinetic and potential) from cardiovascular and echocardiographic variables
#' 
#' @param sbp Brachial systolic blood pressure (mmHg)
#' @param esv LV end-systolic volume (ml)
#' @param dbp Brachial diastolic blood pressure (mmHg)
#' @param edv LV end-diastolic volume (ml)
#' @param ees LV end-sysltolic elastance (mmHg/ml)
#' @param eout Select energy calculation to perform (1 = Ep; 0 = Ek) 1 by default
#'
#' @references
#' Chen, C. H., Fetics, B., Nevo, E., Rochitte, C. E., Chiou, K. R., Ding, P. A., … Kass, D. A. (2001). Noninvasive single-beat determination of left ventricular end-systolic elastance in humans. Journal of the American College of Cardiology, 38(7), 2028–2034. https://doi.org/10.1016/S0735-1097(01)01651-5
#'
#' @export

lvEnergy <- function(sbp, esv, dbp, edv, ees, eout=1){
  if(sum(is.na(c(sbp, esv, dbp, edv, ees, eout)))==0){
    dat <- matrix(c(edv, dbp, esv, sbp), nrow = 2, ncol = 2, byrow = T)
    y.inter <- sbp-(ees*esv)
    x.inter <- -(y.inter)/ees
    if(x.inter < 0){
      area.pot <- (((esv)*(sbp-y.inter))/2) + (y.inter*esv)
      area.kin <- (((edv - esv)*(dbp - sbp))/2) + 
        ((dat[1,1] - dat[2,1])*(dbp))
    } else {
      area.pot <- ((esv - x.inter)*(sbp))/2
      area.kin <- (((edv - esv)*(dbp - sbp))/2) + 
        ((dat[1,1] - dat[2,1])*(dbp))
    }
    if (eout==1){area.pot} else {area.kin}
  }else{
    NA
  }
}
