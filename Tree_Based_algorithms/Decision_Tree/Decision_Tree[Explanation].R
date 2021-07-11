#setwd()
set.seed(123)
Data <- read.csv("DT-Credit.csv", header=TRUE, sep= ";") 
str(Data) 
Data <- Data[,-1]
str(Data)
cols <- c(1, 3:9, 11:21, 23:31)
Data[cols] <- lapply(Data[cols],factor)
attach(Data)
str(Data)
# Change the response to Yes/No answers. 
Target=ifelse(RESPONSE==1,'Yes','No')
Data <- data.frame(Data,Target)
str(Data)
Data=Data[,-31]
attach(Data)
sum( Data$Target=="Yes")
# install.packages("rpart")
library(rpart)
# install.packages ("partykit") 
library("partykit") 

DT_Model_1<-rpart(Target~., data=Data, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 ))

# minsplit: the minimum number of observations that must exist in a node for a new split 
# minbucket: the minimum number of observations in any terminal node 
# Maxdepth: Maximum depth for any node, with the root node counted as depth 0. 

plot(as.party(DT_Model_1))
print(DT_Model_1)
#Explanation of table:
# 1) root 1000 300 Yes (0.3000000 0.7000000)  
# 2) CHK_ACCT=0,1 543 240 Yes (0.4419890 0.5580110)  
# 8) SAV_ACCT=0,1,2 196 74 No (0.6224490 0.3775510) *
# Node 1) shows in whole dataset there are 1000 records.
# In absence of any predictor the optimal decision is
# "1 or Yes". This decision gives us 70% accuracy, and for
# 300 records, which their actual value for the "Target"
# variable are "0 or No" are misclassified.
# Node 2) shows 543 records have "CHK_ACCT=0,1", that for
# 55.8% of them the "Target" variable is "1 or Yes".     
# As 55.8% is the majority, the decision is "1 or Yes".
# This decision results 240 misclassified records,
# which their actual value for the "Target"variable
# are "0 or No". 
# Node 8) with "SAV_ACCT=0,1,2", out of 196 records, for
# 62.2% of them the "Target" variable equals "0 or No".
# Based on the majority rule, there decision is therefore
# "0 or No" that results 74 or 37.7% misclassification.
# The asterisk indicates the terminal node. 


# We now relax the control parameters to get the largest table.
# We are going to "prune" this tree later.
DT_Model_2<-rpart(Target~., data=Data)
plot(as.party(DT_Model_2))
print(DT_Model_2)

# The following line gives us the fitted tree's 
# Complexity Parameter (CP) table.
# Look where you see the least error. 
print(DT_Model_2$cptable) 

# In this table:
# CP stands for the Complexity Parameter. See how it is
# calculated in the note in Blackboard.
# rel error, is the relative error. This is similar to the 
# 1-(R-square) in regression analysis. 
# xerror is the sample mean of the model error using 10-fold
# cross validation (the default in rpart).
# Finally, xstd is the SD of the xerror.

# The line below picks up the least error tree 

opt <- which.min(DT_Model_2$cptable [, "xerror"]) 

# Pruning the tree to the least xerror
cp <- DT_Model_2$cptable [opt,"CP"]
DT_Model_pruned <- prune(DT_Model_2, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = Data ,type = "prob")
Pred_DT_Model_pruned

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "Yes", "No")
Pred_DT_Model_pruned_YN

# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)

# ordering the vectors
Predicted <- ordered(Pred, levels = c("Yes", "No"))
Actual <- ordered(Data$Target,levels = c("Yes", "No"))

# making confusion matrix
# install.packages("caret")
# install.packages("lattice")
# install.packages("ggplot2")
# install.packages("e1071")
library("lattice")
library("ggplot2")
library("caret")
library("e1071")

CM <- confusionMatrix(table(Predicted,Actual))
CM

#	CM description
# For the Table below:
#             Actual
# Predicted	 Yes	No
# Yes 	      A 	B
# No      	  C	  D
# The formulas used are:
# Accuracy = (A+D)/(A+B+C+D)
# random accuracy = ((B+D)(C+D)+(A+C)(A+B))/(A+B+C+D)^2
# kappa = (Accuracy - random accuracy)/(1-random accuracy)
# Sensitivity = A/(A+C)
# Specificity = D/(B+D)
# Prevalence = (A+C)/(A+B+C+D)
# Detection Rate = A/(A+B+C+D)
# Detection Prevalence = (A+B)/(A+B+C+D)
# Balanced Accuracy = (sensitivity+specificity)/2
# Precision = A/(A+B)
# Recall = A/(A+C)
# PPV = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
# NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))

