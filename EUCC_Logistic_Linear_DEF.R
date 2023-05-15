###############################
##LOGISTIC REGRESSION EXAMPLE##
####W/ PREDICTION & TESTING####
###############################

library(caret)
library(rsample)
library(tidymodels)
library(dplyr)

#1)
##imports the data from the local folder##
training=read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTrainingClean70.csv",header = TRUE, sep = "," )
testing0=read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTestingFinal15.csv",header=TRUE,sep=",")
testing=select(testing0,-X.1)
#2)
##builds a logistic regression model
training$stroke<-as.factor(training$stroke)
M1<- glm(training$stroke ~ training$gender+training$age+training$hypertension+training$heart_disease+training$ever_married+ training$work_type+training$Residence_type+training$avg_glucose_level+training$bmi+training$smoking_status, data = training, family = binomial(link="logit"))
summary(M1)
#Run chi-squared test for goodness of fit:
Test_Stat<-M1$null.deviance-M1$deviance #difference in deviance
Test_Stat
Test_df<-M1$df.null-M1$df.residual #difference in degrees of freedom
Test_df
1-pchisq(Test_Stat, Test_df) #p-value for null hypothesis H_0:

#model is significant, the P value is below the threshold

#type="response" to get the in sample predicted probability
View(predict(M1, training, type="response"))
training_prediction<-predict(M1, training,type="response")

##construct confidence intervals:
confint(M1)  #using profiled log-likelihood
confint.default(M1) #using standard errors

##look at confidence interval next to point estimates
point_conf_table<-cbind(M1$coefficients, confint(M1))
point_conf_table 
##converting values to odds-ratio interpretation using base e
exp(point_conf_table)
#3)
###TESTING PHASE & PREDICTION ####
#takes the coefficients to the base e for odds-ratio interpretation#
exp(cbind(M1$coefficients, confint(M1)))
#generating predicted probabilities
predictions<-predict(M1, training, type="response")
View(predictions)

#converts predictions to boolean TRUE (1) or FALSE (0) based on 1/2 threshold on output probability
binpredict <- (predictions >= .5)
View(binpredict)
#build confusion matrix based on binary prediction in-sample
confusion<-table(binpredict, training$stroke == 1)
confusion
#summary analysis of confusion matrix in-sample
confusionMatrix(confusion, positive='TRUE') 

#builds the confusion matrix to look at accuracy on testing data out-of-sample
confusion<-table(binpredict, testing$stroke == 1)
confusion
#summary analysis of confusion matrix out-sample
confusionMatrix(confusion, positive='TRUE') 
###final model validation###
################################################################################
############################### END LOGISTIC ###################################
################################################################################

###############################
#####LINEAR REGRESSION#########
###############################

#1)
##descriptive analysis##
hist(training$avg_glucose_level)
hist(training$bmi)
hist(training$age)
#HISTOGRAM OF avg_glucose_level
plot(training$avg_glucose_level~training$age)
cor(training$avg_glucose_level,training$age) 
ggplot(data=training,aes(x=training$gender,y=training$avg_glucose_level))+
  geom_boxplot()
ggplot(data=training, aes(x=training$smoking_status, y=training$avg_glucose_level))+
  geom_boxplot()
ggplot(data=training, aes(x=training$work_type, y=training$avg_glucose_level))+
  geom_boxplot()
#2)
##modelling##
training$stroke<-as.factor(training$stroke)
mod1=lm(training$avg_glucose_level~training$gender+training$age+training$hypertension+training$heart_disease+training$ever_married+training$work_type+training$Residence_type+training$bmi+training$smoking_status+training$stroke, data=training)
summary(mod1)
###In sample error###
RMSE_IN<-sqrt(sum((pred_in-training$avg_glucose_level)^2)/length(pred_in))
RMSE_IN
#3)
## Predicting and testing##
pred_in<-predict(mod1,training)
PRED_1_OUT <- predict(mod1, testing) 
RMSE_OUT<-sqrt(sum((PRED_1_OUT-testing$avg_glucose_level)^2)/length(PRED_1_OUT))
RMSE_OUT
################################################################################
############################### END LINEAR ###################################
################################################################################
 
##### Final Validation Logistic #####
testing2=read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTestingFinal15.csv",header=TRUE,sep=",")
testing_2=select(testing0,-X.1)
testing_2$stroke<-as.factor(testing_2$stroke)
logistic_predict_final<-predict(M1, testing_2, type="response")
binpredict_final <- (logistic_predict_final >= .5)
confusion_final<-table(binpredict_final, testing_2$stroke == 1)
confusion_final
confusionMatrix(confusion_final, positive='TRUE')

##### Final Validation Linear #####
pred_final<-predict(mod1,testing_2)
RMSE<-sqrt(sum((pred_final-testing_2$avg_glucose_level)^2)/length(pred))
RMSE
