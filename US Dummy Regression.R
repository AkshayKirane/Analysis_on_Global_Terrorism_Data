install.packages('ggplot2')

library(ggplot2)


Data = read.csv('data.csv')
head(Data)
Data$eventid <- NULL

########Categorical Analysis
# convert many distinct values categorical
for (i in colnames(Data)){
  if ((is.numeric(Data[[i]])==FALSE) & (length(unique(Data[[i]]))>=10)){
    Data[[i]][is.na(Data[[i]])] <- 'missing'
    Data[[i]][is.na(Data[[i]])==FALSE] <- 'non-missing'
  } 
}
Data = Data[ , colSums(is.na(Data)) < 100000]
Data$propextent_txt=NULL
Data$attacktype1_txt=NULL
Data$latitude=NULL
Data$longitude=NULL
Data$nwoundte=NULL
Data$nperpcap=NULL
Data$nkillter=NULL
Data$country=NULL
Data$iday=NULL
Data$weapsubtype1=NULL
Data$alternative_txt=NULL
Data$attacktype3_txt=NULL
Data$claimmode3_txt=NULL
Data$weaptype4_txt=NULL
Data$hostkidoutcome_txt=NULL

Data$imonth<- as.numeric(Data$imonth)
Data$region<- as.factor(Data$region)
Data$crit1<- as.factor(Data$crit1)
Data$crit2<- as.factor(Data$crit2)
Data$crit3<- as.factor(Data$crit3)
Data$success<- as.factor(Data$success)
Data$suicide<- as.factor(Data$suicide)
Data$attacktype1<- as.factor(Data$attacktype1)
Data$targtype1<- as.factor(Data$targtype1)
Data$claimed<- as.factor(Data$claimed)
Data$weaptype1<- as.factor(Data$weaptype1)
Data$property<- as.factor(Data$property)
Data$ransom<- as.factor(Data$ransom)
Data$natlty1<- as.factor(Data$natlty1)
Data$targsubtype1<- as.factor(Data$targsubtype1)
Data$ishostkid<- as.factor(Data$ishostkid)
Data$specificity<- as.factor(Data$specificity)
Data$guncertain1<- as.factor(Data$guncertain1)
Data$nperps<- as.numeric(Data$nperps)
Data$nkill<- as.numeric(Data$nkill)
Data$nkillus<- as.numeric(Data$nkillus)
Data$nwound<- as.numeric(Data$nwound)
Data$nwoundus<- as.numeric(Data$nwoundus)
Data$GDP<- as.numeric(Data$GDP)
#Data$nkill[Data$nkill > 0] <- 0



#fill the missing values with mode/mean
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
for (var in 1:ncol(Data)) {
  if (class(Data[,var])=="numeric") {
    Data[is.na(Data[,var]),var] <- median(Data[,var], na.rm = TRUE)
  } else if (class(Data[,var]) %in% c("character", "factor")) {
    Data[is.na(Data[,var]),var] <- Mode(Data[,var], na.rm = TRUE)
  }
}
colSums(is.na(Data))
Data = Data[Data$nkill<=1000,]


#set dummy variables
dumm = as.data.frame(model.matrix(~Data$attacktype1)[,-1])
DummyData = dumm
dumm = as.data.frame(model.matrix(~Data$success)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$suicide)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$crit1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$crit2)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$crit3)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$claimed)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$region)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$targtype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$weaptype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$property)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$ransom)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$natlty1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$targsubtype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$ishostkid)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$specificity)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~Data$guncertain1)[,-1])
DummyData = cbind(DummyData, dumm) 
DummyData = cbind(DummyData, Data$GDP) 
DummyData = cbind(DummyData, Data$nkill)
DummyData = cbind(DummyData, Data$nwound)
DummyData = cbind(DummyData, Data$nperps)
DummyData = cbind(DummyData, Data$nwoundus)
DummyData = cbind(DummyData, Data$nkillus)
DummyData = as.data.frame(DummyData)


######create test and train group
dataframe = DummyData
train_reg <- sample(1:nrow(dataframe), size = 0.75*nrow(dataframe))
val_reg <- -train_reg
data_regtrain = dataframe[train_reg,]
data_valtest = dataframe[val_reg,1:391]
kill_datatest = as.data.frame(dataframe[val_reg,392])
wound_datatest = as.data.frame(dataframe[val_reg,393])

attach(data_regtrain)

#########nkill regression
model1<- lm(Data$nkill~., data=DummyData[,1:391])
summary(model1)
r1=predict(model1, data_valtest)
model1mse = sqrt(mean((r1 - (kill_datatest))^2))
model1mse

qplot(Data$nkill,DummyData$`model.matrix(~Data$suicide)`,geom=c("point","smooth"))


plot(Data$nkill,DummyData$`model.matrix(~Data$suicide)`)
lm(Data$nkill ~ DummyData$`model.matrix(~Data$suicide)`)
abline(lm(Data$nkill ~ DummyData$`model.matrix(~Data$suicide)`))


#########nwound regression
model2<- lm(Data$nwound~., data=DummyData[,1:391])
summary(model2)
r2=predict(model2, data_valtest)
model2mse = sqrt(mean((r2 - (wound_datatest))^2))
model2mse
