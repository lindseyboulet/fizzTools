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
  two <- bind_cols(ldply(strsplit(df$contrast, " - ")), df)
  df <- bind_cols(ldply(strsplit(two[,1], ",")), 
                  ldply(strsplit(two[,2], ",")),
                  df
  )
  split.num <- (ncol(df) - 6)/2
  keepers <- vector()
  for(i in 1:nrow(df)){
    good <- sum(
      array(df[i,1:split.num]) %in%
        array(df[i,(split.num+1):(split.num*2)])
    )
    if(good == split.num-1){
      keepers <- list.append(keepers, i)
    }
  }
  df[keepers,(split.num*2+1):ncol(df)]
}



