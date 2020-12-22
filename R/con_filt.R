#' con_filt {fizzTulz}
#' 
#' @title Returns meaningful contrasts from emmeans output
#'
#' @description Function to filter contrasts from emmeans output based on p-value and that only have on factor difference
#' 
#' @param e1 emmeans output object
#' @param alpha critical p-value to filter contrasts
#' 
#' 
#' @export

con_filt <- function(e1, alpha = 0.05){
  df <- data.frame(e1$contrasts)
  df <- df[complete.cases(df),]
  df <- df[df$p.value<alpha, ]
  df$contrast <- as.character(df$contrast)
  two <-ldply(strsplit(df$contrast, " - ")) %>% 
    apply(2, strsplit, " ")
  comps <- length(two[[1]][[1]]) -1
  keepers <- list()
  for(i in 1:length(two[[1]])){
    if(sum(unlist(two[[1]][i]) %in% unlist(two[[2]][i])) == comps){
      keepers <- list.append(keepers, i)
    }
  }
  df[unlist(keepers),]
}



