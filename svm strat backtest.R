rm(list = ls(all = TRUE))

install.packages("quantmod")
install.packages("e1071")
install.packages("PerformanceAnalytics")
install.packages("ggplot2")
require(quantmod)
require(PerformanceAnalytics)
require(e1071)
require(ggplot2)

getSymbols("^GSPC",from = "2005-01-01", to = "2015-01-01")
Data <- GSPC

cci <- CCI(HLC(Data),n=20,c=0.015)
cmf <- CMF(HLC(Data),Vo(Data),n=20)
cmo <- CMO(Cl(Data),n=14)
dvi <- DVI(Cl(Data))$dvi
macd <- MACD(Cl(Data),nFast=12,nSlow=26,nSig=9)$macd
mfi <- MFI(HLC(Data),Vo(Data),n=14)
obv <- OBV(Cl(Data),Vo(Data))
Momentum <- momentum(Cl(Data))
rsi <- RSI(Cl(Data),n=14)
Stoch <- stoch(HLC(Data),nFastK=14,nFastD=3,nSlowD=3)
fastk <- Stoch$fastK
fastd <- Stoch$fastD
slowd <- Stoch$slowD
tdi <- TDI(Cl(Data),n=20,multiple=2)$tdi
williamsad <- williamsAD(HLC(Data))
wpr <- WPR(HLC(Data),n=14)
#Features from TTR package

Price <- Cl(Data)-Op(Data)
Class <- ifelse(Price>0,"Up","Down")

DataSet <- data.frame(cci,cmf,cmo,dvi,macd,mfi,obv,Momentum,rsi,fastk,fastd,slowd,tdi,williamsad,wpr,Class)
DataSet <- DataSet[-c(1:251),]
colnames(DataSet) <- c("CCI","CMF","CMO","DVI","MACD","MFI","OBV","MOMENTUM","RSI","FASTK","FASTD","SLOWD","TDI","WILLIAMSAD","WPR","Class")

w <- 0.8
Training <- DataSet[1:round(w*length(DataSet[,1])),]
Test <- DataSet[round(w*length(DataSet[,1])+1):length(DataSet[,1]),]
#Separate the data into 80% training set to build our model, 20% test set to test the patterns we found

  
costs <- 0.01
gammas <- 5
kernel <- "radial"
formula = Class~CCI+CMF+DVI+WILLIAMSAD
best.svm <- best.tune(svm,
                      formula,
                      data=Training,kernel = kernel,cost = costs,gamma = gammas,
                      tunecontrol = tune.control(sampling = "cross",cross=5))
#Train SVM

TrainingPredictions<-predict(best.svm,Training,type="class")
TrainingData<-data.frame(Trainingsub,TrainingPredictions)

myPosition <- function(x,model){
  position <- ifelse(predict(model,x,type="class")=="Up",1,-0.5)
  return(position)
}
#Strategy: if the svm model says "Up", long it; else short it.

myStock <- Test
myposition <- myPosition(myStock,best.svm)
bmkReturns <- dailyReturn(Cl(Data[round(w*length(DataSet[,1])+1):length(DataSet[,1]),]), type = "arithmetic")
myReturns <- bmkReturns*Lag(myposition,1)
myReturns[1] <- 0

names(bmkReturns) <- 'SP500'
names(myReturns) <- 'My Strategy'

charts.PerformanceSummary(cbind(bmkReturns,myReturns))
  
Performance <- function(x) {
  
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale=252)
  sharpex = SharpeRatio.annualized(x, scale=252)
  winpctx = length(x[x > 0])/length(x[x != 0])
  annSDx = sd.annualized(x, scale=252)
  
  DDs <- findDrawdowns(x)
  maxDDx = min(DDs$return)
  maxLx = max(DDs$length)
  
  Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
  names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                  "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
  return(Perf)
}
cbind(Me=Performance(myReturns),SP500=Performance(bmkReturns))  


