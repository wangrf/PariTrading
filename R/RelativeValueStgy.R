## using distance method to generate pairs, select top 5 pairs
## when distance diverge to 2 sigma then In, when back to mean then Out



rm(list = ls())

source(file.path(substr(getwd(), 1, 22), "header.R"))


loadRData("sym50")
sym.ben = "000016.SH"
loadRData("000016.SH")

sym50<-sym50[-c(24,28,29,34,33,42,49,50)]

#sym50="600519.SH"

return.nDay = 60
SMA1.nDay=20
peakValley.thresh=20

load("./data/sym50中性策略回测结果.RData")

LSfun<-function(x){
  
  idx1<-match.names("longBuy",names(x))
  idx2<-match.names("longSell",names(x))
  
  y<-ifelse(x[,idx1]==1|x[,idx2]==1,1,0)
  
  a1<-which(y==1)
  
  if(length(a1)%%2==0){
    b1=a1[seq(1,length(a1),2)]
    b2=a1[seq(2,length(a1),2)]
  }else{
    b1=a1[seq(1,length(a1)-1,2)]
    b2=a1[seq(2,length(a1),2)]
  }
  
  z<-rep(0,nrow(x))
  
  myone<-function(up,down){
    up:down
  }
  
  zz<-mapply(myone,b1,b2)
  
  for(i in 1:length(zz)){
    
    z[zz[[i]]]<-i
    
  }

  z<-xts(z,order.by=index(x))
  names(z)<-"LS"
  z
  
}

for(sym in sym50){
  
  applyInd(
    sym,
    fun.name="LSfun",
    arguments = list(
      x=get(sym)
    ),
    label="LSsig"
  )
  
}

LSdata<-merge_ind(symbols = sym50,ind = "LSsig")



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
  alignSymbols(c(sym,sym.ben))

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
  
  
  applyInd(
    sym,
    fun.name = "EMAX",
    arguments = list(
      x = get(sym),
      column = paste0("excess", return.nDay, "D"),
      nDay = SMA1.nDay
    ),
    label = paste0("excess.SMA1", SMA1.nDay, "D")
  )
  
  applyInd(
    sym,
    fun.name = "peakValleyX",
    arguments = list(
      x = get(sym),
      column = paste0("excess.SMA1", SMA1.nDay, "D"),
      thresh=peakValley.thresh
    ),
    label = paste0("peakValley", peakValley.thresh, "D")
  )
  
  
  applyInd(
    sym,
    fun.name = "apply.fromstartX",
    arguments = list(
      x = get(sym),
      column = "peakValley",
      FUN = "PVcount"
    ),
    label = paste0("PVratio")
  )
  
}


PVdata <-
  merge_ind(symbols = sym50,
            ind = paste0("PVnum"))

PVrank<-applyRank(x = PVdata,rankFun = rowRank,descreasing=F)





row.idx<-sapply(sym50,function(symA) ifelse(is.infinite(max(which(get(symA)[,4]==0))),0,max(which(get(symA)[,4]==0)))+1)

excessData<-excessData[,which(row.idx<2500)]
excessData<-excessData["2010-07/"]


excessEMA<-apply(excessData,2,function(x) EMA(x,10,wilder = T))



excessPeak<-apply(excessEMA[-(1:10),],2,findPeaks,thresh=20)
excessValley<-apply(excessEMA[-(1:10),],2,findValleys,thresh=20)
