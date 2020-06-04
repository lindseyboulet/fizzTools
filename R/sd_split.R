#' sd_split {fizzTulz}
#' 
#' @title Split Standard Deviation (error) columns 
#'
#' @description Function that splits standard deviation (error) values in a column into two 
#' new positive and negative columns based on the mean variable value. Used to produce 
#' upper or lower error bars on bar plot. Dataframe should contain  unique mean 
#' and sd columns with matching variable name prefix and differing only by suffix 
#' "_mean" or "_sd" (or "_sem"). Note: If there are any unmatched mean and sd pairs
#' the function will fail. 
#' 
#' @param df dataframe with mean and sd columns. Columns without proper suffix 
#' ("_mean" and "_sd" (or "_sem)) will be ignored. 
#' 
#' 
#' @export

sd_split <- function(df){
  df <- as.data.frame(df)
  col.var <- colnames(df)[grep("_sd", colnames(df))]
  col.edit <- substr(col.var, 1, nchar(col.var)-3)
  suf <- "_sd"
  if(length(col.edit) == 0){
    col.var <- colnames(df)[grep("_sem", colnames(df))]
    col.edit <- substr(col.var, 1, nchar(col.var)-4)
    suf <- "_sem"
  }
  for (i in col.edit){
    pCol <- paste0(i, "_sd_pos")
    nCol <- paste0(i, "_sd_neg")
    df[,c(pCol, nCol)]<-mean(df[,paste0(i, "_mean")])*.05
    for (j in 1:nrow(df)) {
      if(df[j, paste0(i, "_mean")]>0){
        df[j, pCol] <- df[j, paste0(i, suf)]
      }else{
        df[j, nCol] <- df[j, paste0(i, suf)]
      }
    }
  }
  df
}


