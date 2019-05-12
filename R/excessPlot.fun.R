excessPlot <- function(symA, symB="000016.SH", return.nDay=60) {
  
  if (symA %in% ls(envir = .GlobalEnv)) {
    x <- get(symA, envir = .GlobalEnv)
    idx.excess <- grep(paste0("excess", return.nDay, "D"), names(x))
    if (length(idx.excess) == 0) {
      AexcessB(symA, symB, return.nDay , only.last = T)
    }
    
  } else{
    AexcessB(symA, symB, return.nDay , only.last = T)
  }
  
  x <- get(symA, envir = .GlobalEnv)
  idx.excess <- grep(paste0("excess", return.nDay, "D"), names(x))
  
  
  print(plot(x[, c(idx.excess)], main = secName[grep(symA, secName$code), 2]), legend.loc =
          "topleft")
  
  
}
