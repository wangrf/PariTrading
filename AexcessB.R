excessReturn <- function(x, data.ben, column) {
  idx.ben <- match.names(column, colnames(data.ben))
  idx.x <- match.names(column, colnames(x))
  
  y <- x[, idx.x] - data.ben[, idx.ben]
  names(y) <- "excessReturn"
  y
}


AexcessB <-
  function(symA,
           symB,
           return.nDay,
           SMA1.nDay = return.nDay/2,
           SMA2.nDay=return.nDay,
           startDate,
           only.last = T) {
    
    if (length(symB) > 1) {
      stop("symB must be unique")
      
    }
    
    
    sapply(c(symA, symB), loadRData)
    row.idx<- ifelse(is.infinite(max(which(get(symA)[,4]==0))),0,max(which(get(symA)[,4]==0)))
  assign(symA,get(symA)[(row.idx+1):nrow(get(symA)),],envir=.GlobalEnv)
  row.idx<- ifelse(is.infinite(max(which(get(symB)[,4]==0))),0,max(which(get(symB)[,4]==0)))
  assign(symB,get(symB)[(row.idx+1):nrow(get(symB)),],envir=.GlobalEnv)
    
    alignSymbols(c(symA, symB))
    
    for (sym in c(symA, symB)) {
      temp <- get(sym)
      assign(sym, temp[paste0(startDate, "/"), ], envir = .GlobalEnv)
      
    }
    

    

    
    sIdx = 2
    if (only.last) {
      sIdx <- return.nDay
    }
    
    for (sym in c(symA, symB)) {
      applyInd(
        sym,
        fun.name = "Return.calculateX",
        arguments = list(x = get(sym), column = "close"),
        label = "dailyReturn"
      )
      
      for (i in sIdx:return.nDay) {
        applyInd(
          sym,
          fun.name = "rollapplyX",
          arguments = list(
            x = get(sym),
            column = "dailyReturn",
            width = i,
            FUN = "Return.cumulative"
          ),
          label = paste0("cumReturn", i, "D")
        )
        
      }
    }
    
    for (sy in symA) {
      for (i in sIdx:return.nDay) {
        applyInd(
          sy,
          fun.name = "excessReturn",
          arguments = list(
            x = get(sy),
            data.ben = get(symB),
            column = paste0("cumReturn", i, "D")
          ),
          label = paste0("excess", i, "D")
        )
        
        applyInd(
          sy,
          fun.name = "SMAX",
          arguments = list(
            x = get(sy),
            column = paste0("excess", i, "D"),
            nDay = SMA1.nDay
          ),
          label = paste0("excess.SMA1", i, "D")
        )
        
        
        applyInd(
          sy,
          fun.name = "SMAX",
          arguments = list(
            x = get(sy),
            column = paste0("excess", i, "D"),
            nDay = SMA2.nDay
          ),
          label = paste0("excess.SMA2", i, "D")
        )
        
      }
      
      
      
      
    }
    
    
    
    
    
    # for (sy in symA) {
    #   idx <- grep("excess", colnames(get(sy)))
    #   xx <- get(sy)[, idx]
    #   excess.min <- apply(xx, 1, min)
    #   names(excess.min) <- "excess.min"
    #   assign(sy, cbind(get(sy), excess.min))
    #   
    #   idx <- grep("excess", colnames(get(sy)))
    #   xx <- get(sy)[, idx]
    #   excess.max <- apply(xx, 1, max)
    #   names(excess.max) <- "excess.max"
    #   assign(sy, cbind(get(sy), excess.max))
    #   
    # }
    
    
    
    for (sy in symA) {
      assign(sy, get(sy), envir = .GlobalEnv)
      
    }
    assign(symB, get(symB), envir = .GlobalEnv)
    
  }


PairDist<-function(x,column,method="euclidean"){
  
  
}

