#----------------
#RED WINE QUALITY
#----------------
#UE1-ANALYTICAL THEORY AND METHODS - ECONOMETRICS AND STATISTICAL MODELS
#Ivan INGAUD-JAUBERT
#Marina SERRANO DIEGO
#Maéva MECKER

#--------------------------------------------------------------------------------
#######################
#EXPLORATORY ANANLYSIS#
#######################


data<-read.csv('winequality-red.csv')

install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("ggplot2")

library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(ggplot2)

# We will check first what type of variables we have and if they correspond to what we need.

str(data)

# We can observe that they mostly are from type "numeric" which is used for decimal values.
# Except for our quality variable that is categorized as being an "integer" data type.
# However, we would prefer it to be considered as a  "Boolean" type.
# We will base our analysis based on the following measure : a quality from 0 to 5 -> 0 and 6 to 10 -> 1.
# this allows us to transform this variable into a Boolean, and it also allow us to focuse on the wine with the best quality.

data$quality<-as.factor(data$quality)
str(data)
summary(data$quality)

# Let's transform those different levels of factor into only 2 levels in a new variable called "quality_boolean"

data$quality_Boolean[data$quality=="3"] <- "0"
data$quality_Boolean[data$quality=="4"] <- "0"
data$quality_Boolean[data$quality=="5"] <- "0"
data$quality_Boolean[data$quality=="6"] <- "1"
data$quality_Boolean[data$quality=="7"] <- "1"
data$quality_Boolean[data$quality=="8"] <- "1"

str(data)

# Now we will transform our new "quality_boolean" variable into a Boolean :

data$quality_Boolean<-as.factor(data$quality_Boolean)
str(data)
summary(data)

## Analyzing categorical variables
# 'freq' function plots the frequency for our variables :

freq(data$quality_Boolean)

summary(data)

plot_num(data)
# 'plot_num' allows us to have a global view of our data through an historigram plot.
# We can easily see how our data is distributed

# It would now be interesting to see how the distribution of our different variables vary accordingly to the quality of the wine, with quality = 0 and quality = 1.

data_quality <- split.data.frame(data, data$quality_Boolean)

# Here we have all of our variables that are classified with a quality equal to 0.
View(data_quality[["0"]])
plot_num(data_quality[["0"]])

# While here we have all of our variables that are classified with a quality equal to 1.
View(data_quality[["1"]])
plot_num(data_quality[["1"]])

# Now let's look at the differences between the distribution of the different variables for those whom quality = 0 and for quality = 1.
# Since the plot takes a lot of space, we will open each one of them in a new window by using the 'dev.new()' command.
dev.new()
plot_num(data_quality[["0"]]) # opens in "QUARTZ 4"
dev.new()
plot_num(data_quality[["1"]]) # opens in "QUARTZ 5"

# We can observe that "residual.sugar" and "total.sulfur.dioxide" are present in a bigger number of wines that are considered good and have a specific value of 2.5 for "residual.sugar" and 50 for "total.sulfur.dioxide".
# The "alcohol" variable is more homogeneous 

dev.off()
dev.off()
dev.off()

#Check for missing values
install.packages('Amelia')
library(Amelia)
missmap(data, main="Missing values vs observed")

#Check for the colinearity of the regressors
#Correlation matrix

cor(data[c(1:11)])
#The regresors are not highly correlated between each other


#Logistic regression model in order to check the absence of multicolinearity between the regresors

#Run the logistic regression model

wine.model<-glm(quality_Boolean~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                  chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,
                family=binomial(link='logit'), data=data)


#check for the multicolinearity

#DOESNT WORK!! DO WE HAVE TO REMOVE SOME VALUES ??


install.packages ("car")
library(car)

vif(wine.model)


#Split the data set icor.wine.matrix, type="upper", diag=FALSE, tl.col="black", tl.srt = 45)n two
dim(data)

1599*0.66
data.train<-data[1:1055,]
data.test<-data[-c(1:1055),]

#-----------------------------------------------------------------------------------------------------

#######################
# Logistic regression #
#######################

#Run the logistic regression model with all the regressors

wine.model.train<-glm(quality_Boolean~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                        chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,
                      family=binomial(link='logit'), data=data.train)
summary(wine.model.train)

#We will delete all not significant coefficients one by one

#We delete ph (p-value=0.9375>>0.05)

wine.model.train<-glm(quality_Boolean~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                        chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+sulphates+alcohol,
                      family=binomial(link='logit'), data=data.train)
summary(wine.model.train)

#We delete density (p-value=0.4417>>0.05)

wine.model.train<-glm(quality_Boolean~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                        chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates+alcohol,
                      family=binomial(link='logit'), data=data.train)
summary(wine.model.train)

#We delete residual.sugar  (p-value=0.6558>>0.05)

wine.model.train<-glm(quality_Boolean~fixed.acidity+volatile.acidity+citric.acid+
                        chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates+alcohol,
                      family=binomial(link='logit'), data=data.train)
summary(wine.model.train)

#We delete chlorides  (p-value=0.108>>0.05)

wine.model.train<-glm(quality_Boolean~fixed.acidity+volatile.acidity+citric.acid
                      +free.sulfur.dioxide+total.sulfur.dioxide+sulphates+alcohol,
                      family=binomial(link='logit'), data=data.train)
summary(wine.model.train)

##Comment: As all the coefficients are significant We consider this iteration as the last one which means
##that it is our final model.

#--------------------------------------------------------------------------------------------------
##INTERPRETARION OF THE MODEL


#Fixed acidity : The coefficient of the logical regression for the fixed acidity regression is
# 0.18. For every one-unit increase in fixed acidity, the log-odds of the quality of the wine
#being approved increases by 0.18. The positive sign means that the probability of having the quality of
#the red wine accepted will also increase. 

#Volatile acidity : The coefficient of the logical regression for the fixed acidity regression is
# -3.61. For every one-unit decrease in fixed acidity, the log-odds of the quality of the wine
#being approved decreases by 3.61. The negative sign means that the probability of having the quality of
#the red wine accepted will also decrease. 

#Citric acidity : The coefficient of the logical regression for the fixed acidity regression is
# -2.02. For every one-unit decrease in fixed acidity, the log-odds of the quality of the wine
#being approved decreases by 2.02. The negative sign means that the probability of having the quality of
#the red wine accepted will also decrease.

#Free sulfur dioxide : The coefficient of the logical regression for the fixed acidity regression is
#0.02 . For every one-unit increase in fixed acidity, the log-odds of the quality of the wine
#being approved increases by 0.02. The positive sign means that the probability of having the quality of
#the red wine accepted will also increase. 

#Total sulfur dioxide : The coefficient of the logical regression for the fixed acidity regression is
# -0.02. For every one-unit decrease in fixed acidity, the log-odds of the quality of the wine
#being approved decreases by -0.02. The negative sign means that the probability of having the quality of
#the red wine accepted will also decrease.

#Sulphates : The coefficient of the logical regression for the fixed acidity regression is
#1.97. For every one-unit increase in fixed acidity, the log-odds of the quality of the wine
#being approved increases by 1.97. The positive sign means that the probability of having the quality of
#the red wine accepted will also increase.

#Alcohol : The coefficient of the logical regression for the fixed acidity regression is
#0.93. For every one-unit increase in fixed acidity, the log-odds of the quality of the wine
#being approved increases by 0.93. The positive sign means that the probability of having the quality of
#the red wine accepted will also increase.

#Our quality service must focuses on the measure of the following psychicochemical factors
#to forecast the quality of our red wine without having the experts testing:
#-Fixed acidity
#-Volatile acidity
#-Citric acid
#-Free sulfur dioxide
#-Total sulfur dioxide
#-Sulphate
#-Alcohol

#-----------------------------------------------------------------------------------------------------------------

###################
# MODEL ACCURACY #
###################
#We use the second part of our sample "data.test" to estimate the quality of our model performance
#We will compare the predictions on the quality of the red wine and the real results

#We create a new column that shows the probability of quality = 1 based on the model prediction for the test sample

data.test$quality_Boolean.predict<-predict(wine.model.train, newdata=data.test, type='response')
head(data.test$quality_Boolean.predict)

#Transform probabilities of acceptable quality to dichonomic variable (acceptable or not acceptable)
#so we can compare the fitted values with the real values

data.test$quality_Boolean.predict<-ifelse(data.test$quality_Boolean.predict> 0.5,1,0)
head(data.test$quality_Boolean.predict)

#Compare the actual quality results and the quality prediction.

#MisclassificaitonError variable mesures the part of the data that the model predict wrongly

misclassificationError<-mean(data.test$quality_Boolean.predict != data.test$quality_Boolean)
misclassificationError<-round(misclassificationError, digits=2)

#By doing 1-misclassificationError we can obtain the accurancy of the model from a prediction point of view
print(paste('Prediction accurancy=', 1- misclassificationError))

#Comment: We have obtained an 76% of accurancy with this model, wich means that we have a quite good model


#####################
# CONCLUSION MATRIX #
#####################

#We need to use the conclusion matrix to see in more detail where exactly our model is 
#struggeling classifying and forecasting the quality of the red wine

#We give some labels to the variables to easily interpretate the results
data.test$quality_Boolean.predict<-factor(data.test$quality_Boolean.predict, levels=c(0,1), labels=c("predicted bad quality", "predicted good quality"))
data.test$quality_Boolean<-factor(data.test$quality_Boolean, levels=c(0,1), labels=c("Bad quality", "Good quality"))

#We elaborate a comprarision table
tab<-table(data.test$quality_Boolean, data.test$quality_Boolean.predict)
tab

#Comment: We can see that our model is predicting 73 of the bad wines as good and 58 of the good wines as bad.
#The rest of the wine samples are well classified.


#Easily interpreted by looking to the proportions
prop.table(tab,1)

#Comment : We can conclude that our model misclassifies 32% of bad wines and it missclassifies 18% of good wines
#AND 67% of our prediction for bad quality wines are correct and 81% of the predictions for good quality wines are correct

mosaicplot(tab,cex=0.5,color=TRUE)


##############
# PREDICTION #
##############
#We will predict the quality of a specific wine

#Introduce the regressors values

predictors<-data.frame(fixed.acidity=8,volatile.acidity=0.480,citric.acid=0.57,residual.sugar=2.00, 
                       chlorides=0.101,free.sulfur.dioxide=3.0, total.sulfur.dioxide=8,density=0.9970,
                       pH=3.15,sulphates=0.58, alcohol=9.1 )

#We run the prediction using our otimized model
predict(wine.model.train,predictors, type='response')

#Comment: Based on the values (inputs) the probability of being accepted is 0.2368