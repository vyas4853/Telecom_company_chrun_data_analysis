# This is how you call the packages
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)


setwd("G:\\R Language\\R project")

# Reading data; just change the file path to fetch the data.
data <- read.csv("Telco-Customer-Churn.csv")
head(data)
# Data sanity check
str(data)
summary(data)


# Converting necessary variables into factor
data$SeniorCitizen <- as.factor(data$SeniorCitizen)

## Check the missing value (if any)
sapply(data, function(x) sum(is.na(x)))
data<-na.omit(data)
dim(data)


# Logistic Regression on full data

model <- glm(Churn~ gender +	SeniorCitizen + Partner + Dependents + tenure +	PhoneService + MultipleLines
             + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV 
             + StreamingMovies + Contract + PaperlessBilling + PaymentMethod +	MonthlyCharges +	TotalCharges 
             , data=data, family=binomial())
summary(model)

model <- glm(Churn~ 	SeniorCitizen + I(MultipleLines=="Yes")
             + InternetService + I(OnlineSecurity=="Yes")+ I(StreamingTV == "Yes")  
             + I(StreamingMovies=="Yes") + Contract + PaperlessBilling + I(PaymentMethod=="Electronic check")  +	TotalCharges 
             , data=data, family=binomial())
summary(model)


vif(model)




# R square (nagelkarke)
modelChi <- model$null.deviance - model$deviance
#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
R2.hl<-modelChi/model$null.deviance
R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data))
R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data))))))
R.n ## ranges from 0 to 1; closer to 1 better the model
#0.39 not close to one but model is good

############################################################################################################################

# Predicted Probabilities
prediction <- predict(model,newdata = data,type="response")
library(pROC)
rocCurve   <- roc(response = data$Churn, predictor = prediction, 
                  levels = rev(levels(data$Churn)))
data$Churn <- as.factor(data$Churn)
#Metrics - Fit Statistics
predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data$Churn)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

##Gini is 0.65281 as gini is close to 1, so we can predict that model is good
##AUC is 0.8264 , as auc is greater than 0.8 we conclude that trade off between sensitivity and specificity is excellent.
## The sensitivity is defined as the proportion of positive results out of the number 
##of samples which were actually positive.
##When there are no positive results, sensitivity is not defined 
#########################################################################################################################


### KS statistics calculation
data$m1.yhat <- predict(model, data, type = "response")
library(ROCR)
m1.scores <- prediction(data$m1.yhat, data$Churn)
plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")
m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40% - 70%
##Result is ~53% , which lies between 40%-70%  , so model is able to differentiate between good and bad distributors.

######################################################################################################################
names(data)[9] <- "pred"
write.csv(data,"result.csv")
View(data)
