######################
######BA PROJECT######
######################

rm(list=ls(all=TRUE)) # removes prior data from R
library(class)
library(glmnet)
data <- read.csv("data.csv")
head(data)
data$date <- as.Date(with(data, paste(iyear, imonth, iday,sep="-")), "%Y-%m-%d")

# A -----------------------------------------------------------------------


install.packages("ggplot2")
install.packages("reshape2")
install.packages("plyr")
install.packages("languageR")
install.packages("lm4")
install.packages("psych")


# A -----------------------------------------------------------------------


library(ggplot2)
library(plyr)

attack <- matrix(0,9,12)
attack_by_region <- rep(0,12)
weapon <- matrix(0,13,12)
attacksum <- rep(0,9)

#regionname <- unique(data$region_txt)
region_ID <- unique(data$region)
region_ID <- sort(region_ID)
region_name <- c( "North America"=1,"Central America & Carribean"=2,"South America"=3,
                  "East Asia"=4,"Southeast Asia"=5,"South Asia"=6,"Central Asia"=7,
                  "Western Europe"=8,"Eastern Europe"=9, "Middle East & North Africa"=10,
                  "Sub-Saharan Africa"=11,"Australasia & Oceania"=12)


# D -----------------------------------------------------------------------

#New York City insights
years_data <- seq(1,46)
years_data <- years_data + 1970
ny_attacks <- rep(0,46)
new_york <- subset(data, data$city == "New York City")
for(i in 1:46)
{
  ny_attacks[i] <- nrow(subset(new_york, new_york$iyear == years_data[i]))
}
plot(years_data, ny_attacks, type = "l", main="Number of Attacks Vs Year - NYC")

nyc <- data.frame(years_data,ny_attacks)
ny_attacks_decade <- rep(0, 5)
for (i in 1:5)
{
  s <- subset(nyc, 10*(i-1) + 1970 < years_data & years_data <= 10*(i) + 1970)
  ny_attacks_decade[i] <- sum(s$ny_attacks)
}
slices <- ny_attacks_decade
lbls <- c("1970s", "1980s", "1990s", "2000s", "2010s")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls,  col=rainbow(length(lbls)), main="Pie Chart of Attacks over Decades")

# G -----------------------------------------------------------------------

#trying to convert the categorical to binary variables
w <- c(0.1, 0.05, 0.3, 0.1, 0.3, 0.15) #success, region, attack, weapon, target, suicide

success = matrix(0,nrow(data_sig),2)
for (i in 1:nrow(data_sig))
{
  success[i,as.numeric(data_sig$success[i])+1] = w[1]
}

region = matrix(0,nrow(data_sig),12)
for (i in 1:nrow(data_sig))
{
  region[i,as.numeric(data_sig$region[i])] = w[2]
}

attack = matrix(0,nrow(data_sig),9)
for (i in 1:nrow(data_sig))
{
  attack[i,as.numeric(data_sig$attacktype1[i])] = w[3]
}

weapon = matrix(0,nrow(data_sig),13)
for (i in 1:nrow(data_sig))
{
  weapon[i,as.numeric(data_sig$weaptype1[i])] = w[4]
}

target = matrix(0,nrow(data_sig),22)
for (i in 1:nrow(data_sig))
{
  target[i,as.numeric(data_sig$targtype1[i])] = w[5]
}

suicide = matrix(0,nrow(data_sig),2)
for (i in 1:nrow(data_sig))
{
  suicide[i,as.numeric(data_sig$suicide[i])+1] = w[6]
}
# H -----------------------------------------------------------------------

#trying to name the columns and exporting it into a csv file
rm(a)
a = {}
for (i in 1:ncol(success))
{
  a[i] = paste("success",i,sep="")
}
colnames(success) <- a

rm(a)
a = {}
for (i in 1:ncol(suicide))
{
  a[i] = paste("suicide",i,sep="")
}
colnames(suicide) <- a

rm(a)
a = {}
for (i in 1:ncol(target))
{
  a[i] = paste("target",i,sep="")
}
colnames(target) <- a

rm(a)
a = {}
for (i in 1:ncol(region))
{
  a[i] = paste("region",i,sep="")
}
colnames(region) <- a

rm(a)
a = {}
for (i in 1:ncol(attack))
{
  a[i] = paste("attack",i,sep="")
}
colnames(attack) <- a

rm(a)
a = {}
for (i in 1:ncol(weapon))
{
  a[i] = paste("weapon",i,sep="")
}
colnames(weapon) <- a

kmeans_df <- data.frame(success, region, attack, weapon, target, suicide) 
write.csv(kmeans_df, "kmeans.csv")

# I -----------------------------------------------------------------------

#trying to do k-means clustering and getting different clusters

kmeans_data <- read.csv("kmeans.csv")
train_sample1 <- sample(1:nrow(kmeans_data), 0.75*nrow(kmeans_data))
test_sample1 <- -train_sample1
train_sig <- kmeans_data[train_sample1, ]
test_sig <- kmeans_data[test_sample1,]
kmeans_data <- kmeans_data[,-60]

n=4
knn_classification <- knn(train_sig[,-59], test_sig[,-59],train_sig$success1, 2)
k_means <- kmeans(kmeans_data, n)


names(k_means)
kmeans_data$cluster <- k_means$cluster
write.csv(kmeans_data,"kmeans_clustered.csv")

mergeddata$cluster <- k_means$cluster


# I -----------------------------------------------------------------------

#trying to create new sheets for different clusters

for (i in 1:n)
{
  d <- mergeddata[mergeddata$cluster==i,]
  # name <- paste("cluster",i,".csv",sep="")
  #  write.csv(d, name)  
}

#generating plots in project file
for (i in 1:n)
{
  d <- mergeddata[mergeddata$cluster==i,]
  name = paste("C:/E BOOKS/CU Graduate School (2017-2019)/Fall 2017/Business Analytics/Project/Clusters Weighted_4 4/attack",i,".jpeg",sep="")
  jpeg(filename = name)
  hist(d$attacktype1)
  dev.off()
}

#generating pies in project file
for(i in 1:n)
{
  d <- mergeddata[mergeddata$cluster==i,]
  s <- nrow(subset(d, d$success==1))
  f <- nrow(subset(d, d$success==0))
  slices <- c(s,f)
  pct <- round(slices/sum(slices)*100)
  lbls <- c("success", "failure")
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  name = paste("C:/E BOOKS/CU Graduate School (2017-2019)/Fall 2017/Business Analytics/Project/Clusters Weighted_4 4/svf",i,".jpeg",sep="")
  jpeg(filename = name)
  pie(slices, labels = lbls, main="Success vs Failure", col=rainbow(length(lbls)))
  dev.off()  
}

