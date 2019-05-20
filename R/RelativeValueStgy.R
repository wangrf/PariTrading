## using distance method to generate pairs, select top 5 pairs
## when distance diverge to 2 sigma then In, when back to mean then Out



rm(list = ls())

source(file.path(substr(getwd(), 1, 22), "header.R"))


loadRData("sym50")
sym.ben = "000016.SH"

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

LSdatefun<-function(x){
  
  LL<-x[,match.names("LS",names(x))]
  
  ## 距离当期日期最近的进入时点
  L<-diff(LL)
  
  id<-c(1,which(L!=0))
  
  xx<-findInterval(1:length(L),id,rightmost.closed = T)
  
  xx<-xx*as.numeric(LL!=0)
  
  idx<-which(xx!=0)
  
  zz<-rep(as.Date("9999-12-31","%Y-%m-%d"),length(LL))
  
  zz[idx]<-index(LL)[id[xx[idx]]]
  names(zz)<-"LSdate"
  xts(zz,order.by=index(LL))
  
  
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
  applyInd(
    sym,
    fun.name="LSdatefun",
    arguments = list(
      x=get(sym)
    ),
    label="LSdate"
  )
  
  
}

LSdata<-merge_ind(symbols = sym50,ind = "LSdate")

LSdata.rank<-applyRank(x = LSdata,rankFun = rowRank,descreasing=F)
LSdata.rank<-LSdata.rank<=5

LSsig<-merge_ind(symbols=sym50,ind="LSsig")

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
loadRData("000016.SH")

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
    fun.name = "rollapplyX",
    arguments = list(
      x = get(sym),
      column = "peakValley",
      width=500,
      FUN = "PVcount"
    ),
    label = paste0("PVratio")
  )
  
}


PVdata <-
  merge_ind(symbols = sym50,
            ind = paste0("PVratio"))

PVrank<-applyRank(x = PVdata,rankFun = rowRank,descreasing=F)

PVrank<-PVrank<=5

PVrank<-PVrank[index(LSdata)]


xx<-vector(mode = "list",length=ncol(LSdata))

for(i in 1:ncol(LSdata)){

  
  x<-LSsig[,i]
  y<-PVrank[,i]
  f<-LSdata.rank[,i]
  
z<-  unique(x[which((y==1)&(f==1))])
 
 xx[[i]]<-z
}
names(xx)<-names(PVdata)


LS<-xts(matrix(0,nrow(LSdata),ncol(LSdata)),order.by=index(LSdata))

for(i in 1:ncol(LSdata)){
  
  if(length(xx[i])!=0){
    id<-xx[[i]]
    idx<-which(id==0)
    if(length(idx!=0)){
      id<-id[-idx]
      
    }
    
    LS[ which(LSsig[,i]%in%id),i]<-1
    
  }
  
}







RE<-xts(matrix(NA,nrow(LSdata),ncol(LSdata)),order.by=index(LSdata))
dd<-index(RE)

for(i in 1:ncol(LS)){
  
  newR<-get(paste0("R",sym50[i]))
  idx<-which(LS[,i]==1)
  
  RE[idx,i]<-as.numeric(newR[dd[which(LS[,i]==1)]])
  
}



RE<-RE/5
ratio<-apply(!is.na(RE),1,sum)/5
sumRE<-apply(RE,1,sum,na.rm=T)

sumHB<-xts(0.035/365*(1-ratio),order.by=index(sumRE))

sumRE<-sumRE+sumHB

sumRE<-xts(sumRE,order.by=index(RE))
names(sumRE)<-"return"

charts.PerformanceSummary(sumRE)
table.AnnualizedReturns(sumRE)
maxDrawdown(sumRE)
table.Monthly(sumRE)


meanRE<-apply(RE,1,mean,na.rm=T)
meanRE[is.na(meanRE)]<-0.035/365
meanRE[ is.infinite(meanRE)]<-0.035/365
meanRE<-xts(meanRE,order.by=index(RE))
names(meanRE)<-"return"



meanRE<-meanRE["2008/"]

table.AnnualizedReturns(meanRE)
maxDrawdown(meanRE)
table.Monthly(meanRE)
charts.PerformanceSummary(meanRE,wealth.index = T)

sym="600196.SH"
charts.PerformanceSummary(get(paste0("R",sym)),wealth.index = T)
excessPlot(sym)





