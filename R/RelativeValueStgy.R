## using distance method to generate pairs, select top 5 pairs
## when distance diverge to 2 sigma then In, when back to mean then Out



rm(list = ls())

source(file.path(substr(getwd(), 1, 22), "header.R"))


loadRData("sym50")
sym.ben = "000016.SH"
loadRData("000016.SH")

return.nDay = 60

bindData <- function(x, column) {
  idx <- match.names(column, names(x))
  x[, idx]
  
}


Cointegration <- function(x) {
  vals <- data.frame(x)
  beta <-
    ifelse(class(try(coef(lm(
      vals[, 1] ~ vals[, 2] + 0, data = vals
    ))[1], TRUE)
    )  ==  "try-error", NA, coef(lm(
      vals[, 1]  ~  vals[, 2]  +  0, data  =  vals
    ))[1])
  ifelse(is.na(beta), NA, (adf.test(
    vals[, 1] - beta * vals[, 2], alternative = "stationary", k = 0
  ))$p.value)
  
  
}

dtwDistX <- function(x) {
  xx <- t(x)
  ifelse(class(try(dtwDist(xx)[1, 2], TRUE)
  )  ==  "try-error", NA, dtwDist(xx)[1, 2])
  
}

applyInd(
  sym.ben,
  fun.name = "Return.calculateX",
  arguments = list(x = get(sym.ben), column = "close"),
  label = "dailyReturn"
)
applyInd(
  sym.ben,
  fun.name = "rollapplyX",
  arguments = list(
    x = get(sym.ben),
    column = "dailyReturn",
    width = return.nDay,
    FUN = "Return.cumulative"
  ),
  label = paste0("cumReturn", return.nDay, "D")
)



for (sym in sym50) {
  loadRData(sym)
  applyInd(
    sym,
    fun.name = "Return.calculateX",
    arguments = list(x = get(sym), column = "close"),
    label = "dailyReturn"
  )
  
  applyInd(
    sym,
    fun.name = "rollapplyX",
    arguments = list(
      x = get(sym),
      column = "dailyReturn",
      width = return.nDay,
      FUN = "Return.cumulative"
    ),
    label = paste0("cumReturn", return.nDay, "D")
  )
  
  
  applyInd(
    sym,
    fun.name = "bindData",
    arguments = list(
      x = get(sym.ben),
      column = paste0("cumReturn", return.nDay, "D")
    ),
    label = "cumReturn.ben"
    
  )
  
  applyInd(
    sym,
    fun.name = "excessReturn",
    arguments = list(
      x = get(sym),
      data.ben = get(sym.ben),
      column = paste0("cumReturn", return.nDay, "D")
    ),
    label = paste0("excess", return.nDay, "D")
  )
  

  
  #
  # applyInd(
  #
  #   sym,
  #   fun.name="rollapplyX",
  #   arguments=list(
  #
  #     x=get(sym),
  #     column=c(paste0("cumReturn", return.nDay, "D"),"cumReturn.ben"),
  #     width=500,
  #     FUN="dtwDistX"
  #   ),
  #   label="DTW.dist"
  #
  # )
  
  
  
  
}






cumReturn <-
  merge_ind(symbols = sym50,
            ind = paste0("cumReturn", return.nDay, "D"))
