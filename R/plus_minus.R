#' plus_minus {fizzTulz}
#' 
#' @title Table of Mean +/- Values
#'
#' @description Creates mean +/- sd table
#'
#' @param df1 Dataframe with mean columns or with mean and sd columns
#' @param catCols Vector of grouping columns
#' @param digs Number of significant digits 1 by default
#' @param df2 Dataframe with sd columns NA by default
#'
#' @import dply
#'
#' @return Dataframe with mean +/- sd values in character form
#'
#' @examples
#' df1 <- mtcars %>% group_by(gear, carb) %>%
#'   summarise_if(is.numeric, .funs = list("mean" = mean, "sd" = sd))
#'   plusMinus(df1, catCols = c('gear', 'carb'))
#'
#' @export


plus_minus <- function(df1, catCols, digs = 1, df2 = NA){
  if(is.na(df2)){
    df2 <- df1[,c(which(colnames(df1) %in% catCols),grep("_sem",colnames(df1)))]
    if(nrow(df2 <= length(catCols))){
      df2 <- df1[,c(which(colnames(df1) %in% catCols),grep("_sd",colnames(df1)))]
    }
    df1 <- df1[,c(which(colnames(df1) %in% catCols),grep("_mean",colnames(df1)))]
  }

  MdatMean <- as.matrix(df1[,-which(colnames(df1) %in% catCols)])
  MdatSE <- as.matrix(df2[,-which(colnames(df2) %in% catCols)])
  lo <- formatC(MdatMean, format="f", digits=digs)
  hi <- formatC(MdatSE, format="f", digits=digs)
  plusMin <- as.data.frame(matrix( paste(lo, hi, sep=" ± "),
                                   nrow=nrow(MdatMean), dimnames=dimnames(MdatMean)))
  dfTitle <- df1[,catCols]
  cbind(as.data.frame(dfTitle), as.data.frame(plusMin))
}

