US_Data = read.csv('US.csv')
head(US_Data)
US_Data = US_Data[US_Data$nkill<=1000,]

US_Data$alternative=NULL
US_Data$targtype2=NULL
US_Data$iday=NULL
US_Data$Times=NULL
US_Data$crit2=NULL
US_Data$iyear=NULL


###########set data frame
US_Data$imonth<- as.factor(US_Data$imonth)
US_Data$crit1<- as.factor(US_Data$crit1)
US_Data$crit3<- as.factor(US_Data$crit3)
US_Data$success<- as.factor(US_Data$success)
US_Data$suicide<- as.factor(US_Data$suicide)
US_Data$attacktype1<- as.factor(US_Data$attacktype1)
US_Data$targtype1<- as.factor(US_Data$targtype1)
US_Data$claimed<- as.factor(US_Data$claimed)
US_Data$weaptype1<- as.factor(US_Data$weaptype1)
US_Data$property<- as.factor(US_Data$property)
US_Data$ransom<- as.factor(US_Data$ransom)
US_Data$targsubtype1<- as.factor(US_Data$targsubtype1)
US_Data$guncertain1<- as.factor(US_Data$guncertain1)
US_Data$nperps<- as.numeric(US_Data$nperps)
US_Data$nkill<- as.numeric(US_Data$nkill)
US_Data$nwound<- as.numeric(US_Data$nwound)
US_Data$provstate<- as.factor(US_Data$provstate)
US_Data$gname<- as.factor(US_Data$gname)
US_Data$doubtterr<- as.factor(US_Data$doubtterr)
US_Data$weapsubtype1<- as.factor(US_Data$weapsubtype1)
US_Data$propvalue<- as.numeric(US_Data$propvalue)
US_Data$INT_LOG<- as.factor(US_Data$INT_LOG)
US_Data$INT_IDEO<- as.factor(US_Data$INT_IDEO)
US_Data$INT_MISC<- as.factor(US_Data$INT_MISC)
US_Data$INT_ANY<- as.factor(US_Data$INT_ANY)
US_Data$nperpcap<- as.numeric(US_Data$nperpcap)
US_Data$claimmode<- as.factor(US_Data$claimmode)
US_Data$propextent<- as.factor(US_Data$propextent)


#fill the missing values with mode/mean
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
for (var in 1:ncol(US_Data)) {
  if (class(US_Data[,var])=="numeric") {
    US_Data[is.na(US_Data[,var]),var] <- median(US_Data[,var], na.rm = TRUE)
  } else if (class(US_Data[,var]) %in% c("character", "factor")) {
    US_Data[is.na(US_Data[,var]),var] <- Mode(US_Data[,var], na.rm = TRUE)
  }
}

summary(US_Data)

#set dummy variables
dumm = as.data.frame(model.matrix(~US_Data$imonth)[,-1])
DummyData = dumm
dumm = as.data.frame(model.matrix(~US_Data$provstate)[,-1])
DummyData = dumm
dumm = as.data.frame(model.matrix(~US_Data$crit1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$crit3)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$doubtter)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$attacktype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$targtype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$guncertain1)[,-1])
DummyData = cbind(DummyData, dumm)
dumm = as.data.frame(model.matrix(~US_Data$claimed)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$claimmode)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$weaptype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$weapsubtype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$property)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$propextent)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$INT_LOG)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$INT_IDEO)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$INT_MISC)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$INT_ANY)[,-1])
DummyData = cbind(DummyData, dumm)
dumm = as.data.frame(model.matrix(~US_Data$success)[,-1])
DummyData = cbind(DummyData, dumm)
DummyData = cbind(DummyData, US_Data$nperps)
DummyData = cbind(DummyData, US_Data$propvalue)
DummyData = cbind(DummyData, US_Data$nperpcap)
DummyData = cbind(DummyData, US_Data$nkill)
DummyData = cbind(DummyData, US_Data$nwound)
dumm = as.data.frame(model.matrix(~US_Data$suicide)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US_Data$gname)[,-1])
DummyData = cbind(DummyData, dumm)


##########test and train
dataframe = DummyData
train_reg <- sample(1:nrow(dataframe), size = 0.75*nrow(dataframe))
val_reg <- -train_reg
data_regtrain = dataframe[train_reg,]
data_valtest = dataframe[val_reg,-120]
datatest = as.data.frame(dataframe[val_reg,120])




#########decision tree
par(mar=c(3,3,3,3))

attach(data_regtrain)
library(rpart)
orgtree = rpart(model.matrix(~US_Data$gname)[, -1]~US_Data$claimmode+US_Data$weaptype1+US_Data$targtype1+US_Data$attacktype1+US_Data$suicide+US_Data$property+US_Data$claimed+US_Data$weapsubtype1+US_Data$crit1+US_Data$crit3+US_Data$success, data=DummyData[,-120], method = 'class',control=rpart.control(minsplit=20, cp=0.01))
summary(orgtree)
plot(orgtree)
text(orgtree,pretty = 0)
r1=predict(orgtree,data_valtest)
r1_mse = sqrt(mean((r1 - (datatest))^2))
r1_mse
