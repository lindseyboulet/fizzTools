#' silo_select_load {fizzTulz}
#' 
#' @title Combine and Label Silo Selected Output Files  
#'
#' @description Combines files from silo > output > selectData into a single data frame with labels
#' 
#' @param file.dir full 'selectData' directory path
#'
#' @examples
#' silo_select_load('./silo/output/selectData/')
#'
#' @export

silo_select_load <-
  function(file.dir){
    fN <- list.files(file.dir, full.names = TRUE)
    nM <- list.files(file.dir)
    x <- lapply(fN, read.csv)
    nM <- strtrim(nM, nchar(nM)-4)
    names(x) <- nM
    nM2 <- as.data.frame(matrix(as.character(unlist(strsplit(nM, "_"))), 
                                ncol = sapply(gregexpr("_", nM[1]), length) +1, byrow = TRUE))
    nM2 <- cbind(nM2, nM)
    drop.ind <- grep('sec', nM2)[1]
    nM2 <- nM2[,-c(drop.ind:(drop.ind+2))]
    for(i in 1:length(x)){
      df <- x[[i]]
      df$id <- nM2[which(names(x)[i]==nM2[,drop.ind]),1]
      df$cond1 <- nM2[which(names(x)[i]==nM2[,drop.ind]),2]
      if(drop.ind > 3){
        df$cond2 <- nM2[which(names(x)[i]==nM2[,drop.ind]),3] 
      }
      if(drop.ind > 4){
        df$cond3 <- nM2[which(names(x)[i]==nM2[,drop.ind]),4] 
      }
      x[[i]] <- df
    }
    x
  }

