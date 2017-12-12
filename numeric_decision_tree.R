install.packages('party')
install.packages('RWekajars')
install.packages('rJava')
library(rJava)
library(RWekajars)
library(party)
install.packages('tree')
install.packages('RWeka')
install.packages('bit64')
library(tree)
library(data.table)
library(RWeka)
library(bit64)

# Read all data 
gtd_all_attribute <- fread('/Users/akshaykirane/Downloads/gtd.csv')
names(gtd_all_attribute)
attach(gtd_all_attribute)

# read the raw input
cols <- c("eventid","crit1","crit2","crit3" ,"targtype1","targsubtype1","weaptype1","weapsubtype1","country", "attacktype1","attacktype2" , "attacktype3","attacktype1_txt","success","nkill","nhostkid","nwound","nperps","suicide","nperpcap","ndays")
gtd_decision_tree <- fread('/Users/akshaykirane/Downloads/gtd.csv', select = cols)
gtd_decision_tree$eventid = as.factor(gtd_decision_tree$eventid)
class(gtd_decision_tree$eventid)
# Split data into training and testing set
set.seed(3)
gtd_tree_train = sample(1:NROW(gtd_decision_tree) , NROW(gtd_decision_tree)/2) 
gtd_tree_test = - gtd_tree_train 

training_data = gtd_decision_tree[gtd_tree_train,]
test_data = gtd_decision_tree[gtd_tree_test,]


tree_fit <- tree(success~nkill+nwound+nhostkid+nperps+nperpcap+ndays,data = gtd_all_attribute, na.action=NULL)

summary(tree_fit)

# Prediction on test_data
tree_predictions <- predict(tree_fit,test_data )
summary(tree_predictions)
# showing results in matrix format 
table(tree_predictions , test_data$success)
# displaying tree
if(require("party", quietly = TRUE)) plot(tree_fit)
text(j48_fit)

