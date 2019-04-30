#load original
library(readxl)
original_data <- read_excel("~/Dropbox/INFO 6105/info6105-midterm-project/Final_JoinedData_UsingIDColumn.xlsx", 
                            sheet = "filling_with_0_final")
#install.packages('DataExplorer')
library(DataExplorer)
plot_missing(original_data)

# fill the NA with 0
is.na(original_data)
original_data[is.na(original_data)] <- 0
#check the data set
str(original_data)
layout(matrix(c(1),2,2)) # optional 4 graphs/page 
set.seed(1000)

#############################################################################################################
################       convert these categorical variables into numeric       ###############################
#############################################################################################################

#load library
#install.packages("dummies")
#library(dummies)
#create a dummy data frame
#new_original_data <- dummy.data.frame(as.vector(original_data), names = c(
#  "Nationality","Club","Acceleration","Aggression","Agility","Balance",
#  "Ball_control","Composure","Curve","Dribbling","Finishing","Free_kick_accuracy",
#  "GK_diving"," GK_reflexes","Heading_accuracy","Interceptions","Jumping",
#  "Long_passing","Long_shots","Marking","Penalties","Short_passing","Shot_power",
#  "Sliding_tackle","Sprint_speed","Stamina","Standing_tackle","Strength","Vision",
#  "Volleys","Preferred_Positions"))

original_data$Nationality <- as.numeric(as.factor(original_data$Nationality))
original_data$Club <- as.numeric(as.factor(original_data$Club))
original_data$Acceleration <- as.numeric(as.factor(original_data$Acceleration))
original_data$Aggression <- as.numeric(as.factor(original_data$Aggression))
original_data$Agility <- as.numeric(as.factor(original_data$Agility))
original_data$Balance <- as.numeric(as.factor(original_data$Balance))
original_data$Ball_control <- as.numeric(as.factor(original_data$Ball_control))
original_data$Composure <- as.numeric(as.factor(original_data$Composure))
original_data$Curve <- as.numeric(as.factor(original_data$Curve))
original_data$Dribbling <- as.numeric(as.factor(original_data$Dribbling))
original_data$Finishing <- as.numeric(as.factor(original_data$Finishing))
original_data$Free_kick_accuracy <- as.numeric(as.factor(original_data$Free_kick_accuracy))
original_data$GK_diving <- as.numeric(as.factor(original_data$GK_diving))
original_data$GK_handling <- as.numeric(as.factor(original_data$GK_handling))
original_data$GK_reflexes <- as.numeric(as.factor(original_data$GK_reflexes))
original_data$Heading_accuracy <- as.numeric(as.factor(original_data$Heading_accuracy))
original_data$Interceptions <- as.numeric(as.factor(original_data$Interceptions))
original_data$Jumping <- as.numeric(as.factor(original_data$Jumping))
original_data$Long_passing <- as.numeric(as.factor(original_data$Long_passing))
original_data$Long_shots <- as.numeric(as.factor(original_data$Long_shots))
original_data$Marking <- as.numeric(as.factor(original_data$Marking))
original_data$Penalties <- as.numeric(as.factor(original_data$Penalties))
original_data$Short_passing <- as.numeric(as.factor(original_data$Short_passing))
original_data$Shot_power <- as.numeric(as.factor(original_data$Shot_power))
original_data$Sliding_tackle <- as.numeric(as.factor(original_data$Sliding_tackle))
original_data$Sprint_speed <- as.numeric(as.factor(original_data$Sprint_speed))
original_data$Stamina <- as.numeric(as.factor(original_data$Stamina))
original_data$Standing_tackle <- as.numeric(as.factor(original_data$Standing_tackle))
original_data$Strength <- as.numeric(as.factor(original_data$Strength))
original_data$Vision <- as.numeric(as.factor(original_data$Vision))
original_data$Volleys <- as.numeric(as.factor(original_data$Volleys))
original_data$Preferred_Positions <- as.numeric(as.factor(original_data$Preferred_Positions))
original_data$Crossing <- as.numeric(as.factor(original_data$Crossing))
original_data$Positioning <- as.numeric(as.factor(original_data$Positioning))
original_data$Reactions <- as.numeric(as.factor(original_data$Reactions))

#check the data set
str(original_data)

#divide the original data into trainset and testset
train_rows <- sample(nrow(original_data), .8*nrow(original_data))
train <- original_data[train_rows, ]
test <- original_data[-train_rows, ]


#remove the Value(not Value_Num) and identifier(Name) variables
train1 <- subset(train, select = -c(Value, Name))
test1 <- subset(test, select = -c(Value, Name))
str(train1)


#remove the dependent(Value_Num/Value) and identifier(Name) variables
train_para <- subset(train, select = -c(Value_Num, Value, Name))
test_para <- subset(test, select = -c(Value_Num, Value, Name))
str(train_para)


########################################################################################################
###############################    principal component analysis     ####################################
########################################################################################################

prin_comp <- prcomp(train_para, center=TRUE,scale=TRUE)
names(prin_comp)
#outputs the mean of variables
prin_comp$center
#outputs the standard deviation of variables
prin_comp$scale
prin_comp$rotation

dim(prin_comp$x)
#biplot(prin_comp, scale = 0)
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#This plot shows that 30 components results in variance close to ~ 98%. 
#Therefore, in this case, we’ll select number of components as 30 [PC1 to PC30] and proceed to the modeling stage. 
#This completes the steps to implement PCA on train data. 
#For modeling, we’ll use these 30 components as predictor variables and follow the normal procedures.

#add a training set with principal components
train.data <- data.frame(Value_Num = train$Value_Num, prin_comp$x)
#we are interested in first 30 PCAs
train.data <- train.data[,1:31]
train.data

#transform test into PCA
test.data <- predict(prin_comp, newdata = test_para)
test.data <- as.data.frame(test.data)
#select the first 30 components
test.data <- test.data[,1:30]





########################################################################################################
####################################        decision tree          #####################################
########################################################################################################
library(rpart)
library(rpart.plot)

##################################         not use PCA       ############################################

rpart.model1 <- rpart(Value_Num ~ .,data = train1, method = "anova") # all features
rpart.model1
prp(rpart.model1, main = "Value_Num")
rpart.plot(rpart.model1)

# make prediction on test data
rpart.prediction1 <- predict(rpart.model1, test1)
summary(rpart.prediction1)
str(rpart.prediction1)
str(test$Value_Num)

err.rpart1 <- test$Value_Num - rpart.prediction1
rmse.rpart1 <- sqrt(mean((err.rpart^2)))
rmse.rpart1

# error percent
abs.err_rpart1 <- sqrt(err.rpart1^2)
# percent.err_rpart <- mean(abs.err_rpart/test$Value_Num)
percent.err_rpart1 <- abs.err_rpart1/test$Value_Num
percent.err_rpart1
plot(percent.err_rpart1,main="Decision Tree without PCA")

# Errors histogram
hist(err.rpart1, main="Value_Num", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")




#################################        using PCA        ##################################
rpart.model <- rpart(Value_Num ~ .,data = train.data, method = "anova")
rpart.model
prp(rpart.model, main = "Value_Num")
rpart.plot(rpart.model)

# make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)
summary(rpart.prediction)
str(rpart.prediction)
str(test$Value_Num)

err.rpart <- test$Value_Num - rpart.prediction
rmse.rpart <- sqrt(mean((err.rpart^2)))
rmse.rpart

# error percent
abs.err_rpart <- sqrt(err.rpart^2)
#percent.err_rpart <- mean(abs.err_rpart/test$Value_Num)
percent.err_rpart <- abs.err_rpart/test$Value_Num
percent.err_rpart
plot(percent.err_rpart,main="Decision Tree using PCA")

# Errors histogram
hist(err.rpart, main="Decision Tree using PCA", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")





########################################################################################################
###############################        linear regression            ####################################
########################################################################################################

##############################        not use PCA         ####################################

#### step 1 ####
nP_linear.model1 <- lm(train1$Value_Num ~ .,data = train1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(nP_linear.model1)
summary(nP_linear.model1)

# Coefficients: (16 not defined because of singularities) for highly correlated variables

alias(nP_linear.model1)
require(dplyr)
summary(lm(train1$Value_Num ~ .,data = train1))$coefficients
summary(lm(train1$Value_Num ~ .,data = train1))$r.squared


#### step 2 ####
# choose the predictor variables by p-value
# choose all features marked with 3 stars
nP_linear.model2 <- lm(Value_Num ~ Age + Overall + Potential + Special 
                       + Aggression + Agility + Ball_control + Finishing
                       + Long_passing + Marking + Positioning + Stamina
                       + Volleys + CF + CM + LS, data = train1) #not use train1$****
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(nP_linear.model2)
summary(nP_linear.model2)


#### step 3 ####
# choose the predictor variables by p-value
# choose all features marked with 3 stars
nP_linear.model3 <- lm(Value_Num ~ Age + Overall + Potential 
                        + Ball_control  + Marking + Positioning 
                       + Volleys + LS, data = train1) #not use train1$****
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(nP_linear.model3)
summary(nP_linear.model3)


#### step 4 ####
# choose the predictor variables by p-value
# choose all features marked with 3 stars
nP_linear.model4 <- lm(Value_Num ~ Age + Overall + Potential 
                       + Ball_control  + Marking + Positioning 
                       + Volleys, data = train1) #not use train1$****
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(nP_linear.model4)
summary(nP_linear.model4)

#make prediction on test data
nP_linear.prediction <- predict(nP_linear.model2, newdata = test1)
nP_linear.prediction
summary(nP_linear.prediction)
str(nP_linear.prediction)

err.nP_linear <- test1$Value_Num - nP_linear.prediction
rmse.nP_linear <- sqrt(mean((err.nP_linear^2)))
rmse.nP_linear

#error percent
abs.err.nP_linear <- sqrt(err.nP_linear^2)
#percent.err_rpart <- mean(abs.err_rpart/test$Value_Num)
percent.err.nP_linear <- abs.err.nP_linear/test1$Value_Num
percent.err.nP_linear
layout(matrix(c(1),2,2))
plot(percent.err.nP_linear,main="Linear Regression without PCA")

#Errors histogram
layout(matrix(c(1),2,2))
hist(err.nP_linear, main="Linear Regression without PCA", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")




##############     using lasso and ridge to overcome underfitting      ##################
library(glmnet)
library(MASS)
LR.trian <- subset(train1, select = c(Value_Num, Age, Overall, Potential 
                                      , Ball_control, Marking, Positioning 
                                      , Volleys))
#x.train <- as.matrix(LR.trian[,-1])
#y.train <- as.double(as.matrix(LR.trian[, 1]))
x.train <- as.matrix(train1[,-68])
y.train <- as.matrix(train1[,68])


LR.test <- subset(test1, select = c(Value_Num, Age, Overall, Potential 
                                      , Ball_control, Marking, Positioning 
                                      , Volleys))
#x.test <- as.matrix(LR.test[,-1]) 
#y.train <- as.double(as.matrix(LR.trian[, 1]))
x.test <- as.matrix(test1[,-68])

#########################     Ridge       #################################

NP.ridge <- cv.glmnet(x.train, y.train, alpha=0)
opt_lambda <- NP.ridge$lambda.min
opt_lambda
coef(NP.ridge, s=NP.ridge$lambda.min)
plot(NP.ridge)
#summary(NP.ridge)

#make prediction on test data
NP.ridge.prediction <- predict(NP.ridge, x.test)
NP.ridge.prediction
summary(NP.ridge.prediction)

err.NP.ridge <- test1$Value_Num - NP.ridge.prediction
rmse.NP.ridge <- sqrt(mean((err.NP.ridge^2)))
rmse.NP.ridge

#error percent
abs.err.NP.ridge <- sqrt(err.NP.ridge^2)
#percent.err_rpart <- mean(abs.err_rpart/test$Value_Num)
percent.err.NP.ridge <- abs.err.NP.ridge/test1$Value_Num
percent.err.NP.ridge
layout(matrix(c(1),2,2))
plot(percent.err.NP.ridge,main="Ridge without PCA")

#Errors histogram
layout(matrix(c(1),2,2))
hist(err.NP.ridge, main="Ridge without PCA", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")


#########################     Lasso      ####################################

NP.lasso <- cv.glmnet(x.train, y.train, alpha=1)
opt_lambda1 <- NP.lasso$lambda.min
opt_lambda1
coef(NP.lasso, s=NP.lasso$lambda.min)
plot(NP.lasso)
#summary(NP.lasso)

#make prediction on test data
NP.lasso.prediction <- predict(NP.lasso, x.test)
NP.lasso.prediction
summary(NP.lasso.prediction)

err.NP.lasso <- test1$Value_Num - NP.lasso.prediction
rmse.NP.lasso <- sqrt(mean((err.NP.lasso^2)))
rmse.NP.lasso

#error percent
abs.err.NP.lasso <- sqrt(err.NP.lasso^2)
#percent.err_rpart <- mean(abs.err_rpart/test$Value_Num)
percent.err.NP.lasso <- abs.err.NP.lasso/test1$Value_Num
percent.err.NP.lasso
layout(matrix(c(1),2,2))
plot(percent.err.NP.lasso,main="Lasso without PCA")

#Errors histogram
layout(matrix(c(1),2,2))
hist(err.NP.lasso, main="Lasso without PCA", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")






#################################         use PCA         ########################################

P_linear.model <- lm(Value_Num ~ .,data = train.data)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(P_linear.model)
summary(P_linear.model)

#make prediction on test data
P_linear.prediction <- predict(P_linear.model, newdata = test.data)
P_linear.prediction
summary(P_linear.prediction)
str(P_linear.prediction)

err.P_linear <- test1$Value_Num - P_linear.prediction
rmse.P_linear <- sqrt(mean((err.P_linear^2)))
rmse.P_linear

#error percent
abs.err.P_linear <- sqrt(err.P_linear^2)
#percent.err_rpart <- mean(abs.err_rpart/test$Value_Num)
percent.err.P_linear <- abs.err.P_linear/test1$Value_Num
percent.err.P_linear
layout(matrix(c(1),2,2))
plot(percent.err.P_linear,main="Linear Regression using PCA")

#Errors histogram
layout(matrix(c(1),2,2))
hist(err.P_linear, main="Linear Regression using PCA", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")






########################################################################################################
###############################            Random Forest              ##################################
########################################################################################################

# install.packages("rsample")
# install.packages("randomForest")
# install.packages("caret")
library(randomForest)
library("csv")
library("rsample")
library(tidyr)
fit.rf <- randomForest(Value_Num ~ Age +Overall, data = train1)
fit.rf
plot(fit.rf)

varImpPlot(fit.rf,
           sort = T,
           main="Variable Importance")
print("importance of variable")

importance(fit.rf)



pred.rf <- predict(fit.rf, test1)
err.rf <- (pred.rf) - test1$Value_Num
rmse.rf <- sqrt(mean((err.rf)^2))
rmse.rf
#c(RMSE = rmse.rf, pseudoR2 = mean(fit.rf$rsq))
plot(pred.rf,test1$Value_Num, xlab = "Error", ylab = "Value of Player")

#Errors histogram
layout(matrix(c(1),2,2))
hist(err.rf, main="Value_Num", xlab="Error", breaks=10, col="darkred")






########################################################################################################
##################################          XG boost          ##########################################
########################################################################################################

library(xgboost)
library('Matrix')

colnames(train1)
Xg_Data<-train1[,c("Age","Overall","Value_Num")]
m<-Matrix(as.matrix(Xg_Data[,1:2]), sparse = T)
Xg_Data_Test<-test1[,c("Age","Overall","Value_Num")]
m_test<-Matrix(as.matrix(Xg_Data_Test[,1:2]), sparse = T)

model<- xgboost(data= m,label= train1$Value_Num,nround=10,objective="reg:linear")#reg:linear
model

pred<- predict(model,m_test)
rm<-pred-test1$Value_Num
abs.rm= sqrt(rm^2)
mean.err = mean(abs.rm/test1$Value_Num)
rmse <- sqrt(mean((rm^2)))
sprintf("RMSE of train data %f",rmse) #12.92


hist(rm, main="XGBoost", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")


