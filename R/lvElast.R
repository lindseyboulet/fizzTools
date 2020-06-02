#' lvElast {fizztools}
#' 
#' @title Non-Invasive Left Ventricular End-Systolic Elastance
#'
#' @description Calculates End Systolic Elastance from cardiovascular and echocardiographic variables
#' 
#' @param rtonset Time from R-wave onset to aortic opening (s)
#' @param rtend Time from R-wave onset to aortic closing (s)
#' @param ef Ejection fraction
#' @param dbp Brachial diastolic blood pressure (mmHg)
#' @param sbp Brachial systolic blood pressure (mmHg)
#' @param sv Stroke volume (ml)
#'
#' @references
#' Chen, C. H., Fetics, B., Nevo, E., Rochitte, C. E., Chiou, K. R., Ding, P. A., … Kass, D. A. (2001). Noninvasive single-beat determination of left ventricular end-systolic elastance in humans. Journal of the American College of Cardiology, 38(7), 2028–2034. https://doi.org/10.1016/S0735-1097(01)01651-5
#'
#' @export

lvElast <- function(rtonset, rtend, ef, dbp, sbp, sv){
  if(sum(is.na(c(rtonset, rtend, ef, dbp, sbp, sv)))==0){
    tnd <- rtonset/rtend
    if(ef>1){ef<- ef/100}
    sbp_est <- sbp*.9
    poly <- c(0.35695, -7.2266, 74.249, -307.39, 684.54, 
              -856.92, 571.95, -159.1)
    polyev <- vector()
    for (i in 1:8){
      p <- i-1
      polyev <- append(polyev, poly[i]*tnd^p)
    }
    Endav <- sum(polyev)
    Endes <- 0.0275-(0.165*ef)+(0.3656*(dbp/sbp_est))+(0.515*Endav)
    (dbp - (Endes * sbp_est))/(sv * Endes)
  }else{
    NA
  }
}


