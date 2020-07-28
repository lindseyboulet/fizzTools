#' eff_filt {fizzTulz}
#' 
#' @title Filter simple effects from emmeans object
#'
#' @description Pulls contrasts from emmeans object containing an interaction of two or more categorical 
#' variables where there is only one factor difference
#'
#' @param e1 Emmeans output object
#' @param p P-value to filter significant contrasts 0.05 by default 
#'
#' @return A dataframe of meaningful contrasts
#'
#' @importFrom plyr ddply
#'
#' @export


eff_filt <- function(e1, p = 0.05){
  c1 <- data.frame(e1$contrasts)
  c1 <- c1[c1$p.value<p,]
  conts <- ldply(strsplit(as.character(c1$contrast)," - "))
  c1$keep <- 0
  for(i in 1:nrow(conts)){
    left <- unlist(strsplit(conts[i,1], ","))
    if(sum(left %in% unlist(strsplit(conts[i,2], ",")))==length(left)-1){
      c1$keep[i] <- 1
    }
  }
  c1[c1$keep==1,1:6]
}

