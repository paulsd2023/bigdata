###############################
##LOGISTIC REGRESSION EXAMPLE##
####W/ PREDICTION & TESTING####
###############################

library(caret)
library(rsample)
library(tidymodels)
#1)
##imports the data from the local folder##
training=read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTrainingClean70.csv",header = TRUE, sep = "," )
testing0=read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTestingClean30.csv",header=TRUE,sep=",")


p<-.5 
obs_count<-dim(testing0)[1]
testing_size <- floor(p * obs_count)
testing_size
#SET THE RANDOM SEED FOR REPRODUCIBILITY
set.seed(12345)


testing_ind <- sample(obs_count, size = testing_size)
testing_1 <- testing0[testing_ind, ] 
testing_2 <- testing0[-testing_ind, ] ##partitioning the testing sample in two half##


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
#the x-variables are not useful predictors of the categorical variable

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

#### ISSUE ####

#converts predictions to boolean TRUE (1) or FALSE (0) based on 1/2 threshold on output probability
binpredict <- (predictions >= .5)
View(binpredict)

#build confusion matrix based on binary prediction in-sample
confusion<-table(binpredict, training$stroke == 1)
confusion

#summary analysis of confusion matrix in-sample
confusionMatrix(confusion, positive='TRUE') 

#builds the confusion matrix to look at accuracy on testing data out-of-sample
confusionMatrix(table(predict(M1, data=testing_1, type="response") >= 0.5,
                      testing_1$stroke == 1), positive = 'TRUE')

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
#3)
## Predicting and testing##
pred_in<-predict(mod1,training)
View(pred_in)
View(mod1$fitted.values)
### ISSUE IN TESTIN ###
pred_out<-predict(mod1,testing_1)
head(training$work_type)
View(training$work_type)