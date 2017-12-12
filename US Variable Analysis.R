install.packages('caret')
install.packages('class')



############Domestic Variables Analysis

#Read csv
US_Data = read.csv('US.csv')
head(US_Data)


Unemployment = read.csv('unemployment rate.csv')
US1 = merge(US_Data,Unemployment,by=c("iyear","imonth","provstate"),all = F)

Race = read.csv('Race.csv')
US2 = merge(US1,Race,by=c("provstate"),all.x = FALSE, all.y = F)

Poverty = read.csv('Poverty.csv')
colnames(Poverty)[1] <- 'provstate' 
US3 = merge(US2,Poverty,by=c("provstate","iyear"),all = F)

Vacancy = read.csv('Vacancy.csv')
colnames(Vacancy)[1] <- 'provstate' 
US4 = merge(US3,Vacancy,by=c("provstate","iyear"),all = F)

#create dataframe
US4$alternative=NULL
US4$targtype2=NULL
US4$iday=NULL
US4$crit2=NULL

US4$imonth<- as.factor(US4$imonth)
US4$crit1<- as.factor(US4$crit1)
US4$crit3<- as.factor(US4$crit3)
US4$success<- as.factor(US4$success)
US4$suicide<- as.factor(US4$suicide)
US4$attacktype1<- as.factor(US4$attacktype1)
US4$targtype1<- as.factor(US4$targtype1)
US4$claimed<- as.factor(US4$claimed)
US4$weaptype1<- as.factor(US4$weaptype1)
US4$property<- as.factor(US4$property)
US4$ransom<- as.factor(US4$ransom)
US4$targsubtype1<- as.factor(US4$targsubtype1)
US4$guncertain1<- as.factor(US4$guncertain1)
US4$nperps<- as.numeric(US4$nperps)
US4$nkill<- as.numeric(US4$nkill)
US4$nwound<- as.numeric(US4$nwound)
US4$provstate<- as.factor(US4$provstate)
US4$gname<- as.factor(US4$gname)
US4$doubtterr<- as.factor(US4$doubtterr)
US4$weapsubtype1<- as.factor(US4$weapsubtype1)
US4$propvalue<- as.numeric(US4$propvalue)
US4$INT_LOG<- as.factor(US4$INT_LOG)
US4$INT_IDEO<- as.factor(US4$INT_IDEO)
US4$INT_MISC<- as.factor(US4$INT_MISC)
US4$INT_ANY<- as.factor(US4$INT_ANY)
US4$nperpcap<- as.numeric(US4$nperpcap)
US4$claimmode<- as.factor(US4$claimmode)
US4$propextent<- as.factor(US4$propextent)
US4$iyear<- as.numeric(US4$iyear)
US4$UER<- as.numeric(as.character(US4$UER))

#fill the missing values with mode/mean
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
for (var in 1:ncol(US4)) {
  if (class(US4[,var])=="numeric") {
    US4[is.na(US4[,var]),var] <- median(US4[,var], na.rm = TRUE)
  } else if (class(US4[,var]) %in% c("character", "factor")) {
    US4[is.na(US4[,var]),var] <- Mode(US4[,var], na.rm = TRUE)
  }
}
summary(US4)

#set dummy variables
dumm = as.data.frame(model.matrix(~US4$imonth)[,-1])
DummyData = dumm
dumm = as.data.frame(model.matrix(~US4$provstate)[,-1])
DummyData = dumm
dumm = as.data.frame(model.matrix(~US4$crit1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$crit3)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$doubtter)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$suicide)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$attacktype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$targtype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$guncertain1)[,-1])
DummyData = cbind(DummyData, dumm)
dumm = as.data.frame(model.matrix(~US4$claimed)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$claimmode)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$weaptype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$weapsubtype1)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$property)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$propextent)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$INT_LOG)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$INT_IDEO)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$INT_MISC)[,-1])
DummyData = cbind(DummyData, dumm) 
dumm = as.data.frame(model.matrix(~US4$INT_ANY)[,-1])
DummyData = cbind(DummyData, dumm)
dumm = as.data.frame(model.matrix(~US4$success)[,-1])
DummyData = cbind(DummyData, dumm)
dumm = as.data.frame(model.matrix(~US4$gname)[,-1])
DummyData = cbind(DummyData, dumm)
DummyData = cbind(DummyData, US4$iyear)
DummyData = cbind(DummyData, US4$nperps)
DummyData = cbind(DummyData, US4$propvalue)
DummyData = cbind(DummyData, US4$nperpcap)
DummyData = cbind(DummyData, US4$nkill)
DummyData = cbind(DummyData, US4$nwound)
DummyData = cbind(DummyData, US4$UER)
DummyData = cbind(DummyData, US4$White)
DummyData = cbind(DummyData, US4$Black)
DummyData = cbind(DummyData, US4$Hispanic)
DummyData = cbind(DummyData, US4$Asian)
DummyData = cbind(DummyData, US4$Percent)
DummyData = cbind(DummyData, US4$Vacancy)
DummyData = cbind(DummyData, US4$Times)



#Regression for times
par(mar=c(4,4,4,4))

dataframe = DummyData
train_reg <- sample(1:nrow(dataframe), size = 0.75*nrow(dataframe))
val_reg <- -train_reg
data_regtrain = dataframe[train_reg,]
data_valtest = dataframe[val_reg,-123]
datatest = as.data.frame(dataframe[val_reg,123])

attach(data_regtrain)
Black_reg<- lm(US4$Times~US4$Black, data=data_regtrain[,116:123])
plot(US4$Times,US4$Black)
abline(Black_reg)
r1=predict(Black_reg, data_valtest)
Black_regmse = sqrt(mean((r1 - (datatest))^2))
Black_regmse

White_reg<- lm(US4$Times~US4$White, data=data_regtrain[,116:123])
plot(US4$Times,US4$White)
abline(White_reg)
r2=predict(White_reg, data_valtest)
White_regmse = sqrt(mean((r2 - (datatest))^2))
White_regmse

Hispanic_reg<- lm(US4$Times~US4$Hispanic, data=data_regtrain[,116:123])
plot(US4$Times,US4$Hispanic)
abline(Hispanic_reg)
r3=predict(Hispanic_reg, data_valtest)
Hispanic_regmse = sqrt(mean((r3 - (datatest))^2))
Hispanic_regmse

Asian_reg<- lm(US4$Times~US4$Asian, data=data_regtrain[,116:123])
plot(US4$Times,US4$Asian)
abline(Asian_reg)
r4=predict(Asian_reg, data_valtest)
Asian_regmse = sqrt(mean((r4 - (datatest))^2))
Asian_regmse

Poverty_reg<- lm(US4$Times~US4$Percent, data=data_regtrain)
plot(US4$Times,US4$Percent)
abline(Poverty_reg)
summary(Poverty_reg)
r5=predict(Poverty_reg, data_valtest)
Poverty_regmse = sqrt(mean((r5 - (datatest))^2))
Poverty_regmse

Vacancy_reg<- lm(US4$Times~US4$Vacancy, data=data_regtrain)
plot(US4$Times,US4$Vacancy)
abline(0,1)
summary(Vacancy_reg)
r6=predict(Vacancy_reg, data_valtest)
Vacancy_regmse = sqrt(mean((r6 - (datatest))^2))
Vacancy_regmse

UER_reg<- lm(US4$Times~US4$UER, data=data_regtrain)
plot(US4$Times,US4$UER)
abline(UER_reg)
summary(UER_reg)
uerrr=predict(UER_reg, data_valtest)
UER_regmse = sqrt(mean((uerrr - (datatest))^2))
UER_regmse

myreg<- lm(US4$Times~US4$UER+US4$White+US4$Black+US4$Hispanic+US4$Asian+US4$Percent+US4$Vacancy, data=data_regtrain)
summary(myreg)
r7=predict(myreg, data_valtest)
myregmse = sqrt(mean((r7 - (datatest))^2))
myregmse

all_reg<- lm(US4$Times~US4$UER*US4$White+US4$UER*US4$Black+US4$UER*US4$Hispanic+US4$UER*US4$Asian+US4$UER*US4$Percent+US4$UER*US4$Vacancy+US4$Vacancy*US4$Percent+US4$White*US4$Vacancy+US4$Black*US4$Vacancy+US4$Hispanic*US4$Vacancy+US4$Asian*US4$Vacancy+US4$White*US4$Percent+US4$Black*US4$Percent+US4$Hispanic*US4$Percent+US4$Asian*US4$Percent, data=data_regtrain[,116:123])
summary(all_reg)
r8=predict(all_reg, data_valtest)
all_regmse = sqrt(mean((r8 - (datatest))^2))
all_regmse


#Regression for success rate
plot(US4$success,US4$White,xlab='success',ylab='Proportion of White in a state')
plot(US4$success,US4$Black,xlab='success',ylab='Proportion of Black in a state')
plot(US4$success,US4$Hispanic,xlab='success',ylab='Proportion of Hispanic in a state')
plot(US4$success,US4$Asian,xlab='success',ylab='Proportion of Asian in a state')
plot(US4$success,US4$UER,xlab='success',ylab='Unemployment Rate')
plot(US4$success,US4$Percent,xlab='success',ylab='Poverty Percent')
plot(US4$success,US4$Vacancy,xlab='success',ylab='Homeownership Rate')

logit=glm(US4$success~US4$UER+US4$Black+US4$White+US4$Asian+US4$Hispanic+US4$Percent+US4$Vacancy,data=data_regtrain,family=binomial)
summary(logit)
r9=predict(logit, data_valtest)
logitmse = sqrt(mean((r9 - (datatest))^2))
logitmse



#####################knn
library(caret)
newdf <- as.data.frame(US4$provstate)
newdf = cbind(newdf,US4[,30:37])
newdf$`US4$provstate` = as.factor(newdf$`US4$provstate`)

train_ind <- sample(seq_len(nrow(newdf)), size = 0.75*nrow(newdf))
data_train = newdf[train_ind,]
data_test = newdf[-train_ind,]


#train <- sample(1:nrow(newdf), size = 0.75*nrow(newdf))
#val <- -train
kmeans_data <- newdf[,2:9]
#data_train = newdf[train,]
#data_test = newdf[val,]
xtrain <- data_train[,2:9]
ytrain <- data_train[,1]
xtest <- data_test[,2:9]
ytest <- data_test[,1]
library(class)
new_data = knn(xtrain, xtest, ytrain , k = 5)

sqrt(mean((as.numeric(new_data) - as.numeric(ytest))^2)) -> a
a


####################kmeans
fff <- kmeans(kmeans_data,5)
summary(fff)
names(fff)
fff$cluster
fff
