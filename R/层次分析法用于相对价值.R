
syms<-c("000016.SH","000300.SH","000905.SH","399006.SZ")
nSym<-length(syms)

mat<-matrix(0,nrow=nSym,ncol=nSym)

source(file.path(substr(getwd(),1,22),"header.R"))



#correct for TZ issues if they crop up
oldtz<-Sys.getenv('UTC')
if(oldtz=='') {
  Sys.setenv(TZ="UTC")
}
r.nDay=60


for(i in 1:(nSym-1)){
  for(j in min(i+1,nSym):nSym){
  
    symA<-syms[i]
    symB<-syms[j]
  print(c(symA,symB)  )
    AexcessB(symA,symB,return.nDay=r.nDay,SMA1.nDay = round(r.nDay/2,0),SMA2.nDay = r.nDay,only.last=T)
    applyInd(sym=symA,fun.name="rollapplyX",
             arguments=list(x=get(symA),column=paste0("excess",r.nDay,"D"),width=500,FUN="last.percentile"),
             label="percent")
    
  assign(paste0("pert",i,j),1/get(symA)[,ncol(get(symA))])
    rm(list=c(symA,symB))
    
  }
}

for(i in 1:nSym){
  assign(paste0("pert",i,i),xts(rep(1,nrow(pert12)),order.by=index(pert12)))
  
}
pert21<-1/pert12
pert31<-1/pert13
pert32<-1/pert23
pert41<-1/pert14
pert42<-1/pert24
pert43<-1/pert34


sn<-ls(pattern="pert")
for(sk in sn){
  xx<-get(sk)
  names(xx)<-sk
  assign(sk,xx)
}

alignSymbols(ls(pattern = "pert"))

mm<-cbind(pert11,pert12)
for(sk in sn[-c(1,2)]){
  
  mm<-cbind(mm,get(sk))
}


weight <- function (judgeMatrix, round=3) {
  n = ncol(judgeMatrix)
  cumProd <- vector(length=n)
  cumProd <- apply(judgeMatrix, 1, prod)  ##求每行连乘积
  weight <- cumProd^(1/n)  ##开n次方(特征向量)
  weight <- weight/sum(weight) ##求权重
  round(weight, round)
}

myweight<-function(x){
  
  ss<-matrix(x,nrow=nSym,ncol=nSym,byrow=T)
  weight(ss)
  
}

out<-t(apply(mm,1,myweight))
out<-xts(out,order.by=index(mm))
names(out)<-syms
plot(out,legend.loc="topleft")







# 
# for(sym in syms){
#   
# loadRData(sym)  
#   applyInd(
#     sym,
#     fun.name = "Return.calculateX",
#     arguments = list(x = get(sym), column = "close"),
#     label = "dailyReturn"
#   )
#   
#   applyInd(sym=sym,fun.name="rollapplyX",
#            arguments=list(x=get(sym),column="dailyReturn",width=5,FUN="Return.cumulative",partial=F),
#            label=paste0("cum","Return",5))
#   
#   
#   
#   applyInd(sym=sym,fun.name="leadX",
#            arguments=list(x=get(sym),column=paste0("cum","Return",5),k=5),
#            label=paste0("RleadCum",5))
# }
# 
# 
# ret<-cbind(get(syms[1])[,ncol(get(syms[1]))],
#       get(syms[2])[,ncol(get(syms[2]))],
#       get(syms[3])[,ncol(get(syms[3]))],
#       get(syms[4])[,ncol(get(syms[4]))])
# 
# tt<-index(ret)[Tcalendar(ret,daysInter="weekfirst")==1]
# 
# out<-out[tt,]
# out<-out["2013/"]
# ret<-ret[index(out),]
# 
# R.stgy<-xts(diag(ret%*%t(out)),order.by=index(ret))
# RR<-cbind(R.stgy,ret)
# charts.PerformanceSummary(RR["2014/"], geometric=FALSE, wealth.index=TRUE,main = "反转净值曲线")
# table.AnnualizedReturns(R = RR["2014/"])

