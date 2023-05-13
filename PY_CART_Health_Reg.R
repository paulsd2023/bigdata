######################################
##HEALTH TREE-BASED MODELS: CART##
######################################

#LOADING THE LIBRARIES
library(tidymodels) #INCLUDES parsnip PACKAGE FOR decision_tree()
library(caret) #FOR confusionMatrix()
library(rpart.plot)
##LOAD THE GGPLOT2, DPLYR, PLYR, TSERIES, and GGTHEMES LIBRARIES
library(ggplot2) #for ggplot system and preloaded datasets
library(dplyr) #for more data manipulation than plyr
library(plyr) #for ddply()
library(tseries) #for the J-B test
library(ggthemes) #for preloaded themes for ggplot2

#IMPORTING THE DATA
#IMPORTING THE DATA
df1 <- read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTrainingClean70.csv", header=TRUE)

df1$gender<-as.factor(df1$gender) #CONVERT TO FACTOR
df1$hypertension<-as.factor(df1$hypertension) #CONVERT TO FACTOR
df1$heart_disease<-as.factor(df1$heart_disease) #CONVERT TO FACTOR
df1$ever_married<-as.factor(df1$ever_married) #CONVERT TO FACTOR
df1$work_type<-as.factor(df1$work_type) #CONVERT TO FACTOR
df1$Residence_type<-as.factor(df1$Residence_type) #CONVERT TO FACTOR
df1$smoking_status<-as.factor(df1$smoking_status) #CONVERT TO FACTOR
df1$stroke<-as.factor(df1$stroke) #CONVERT TO FACTOR

summary(df1)#examine the data summary

##PARTITIONING THE DATA##
set.seed(123)
split<-initial_split(df1, prop=.7, strata=stroke)
train<-training(split)
test<-testing(split)

#SPECIFYING THE REGRESSION TREE MODEL
reg_spec <- decision_tree(min_n = 10 , #minimum number of observations for split
                           tree_depth = 30, #max tree depth
                           cost_complexity = 0.008)  %>% #regularization parameter
            set_engine("rpart") %>%
            set_mode("regression")
print(reg_spec)

#ESTIMATING THE MODEL (CAN BE DONE IN ONE STEP ABOVE WITH EXTRA %>%)
reg_fmla <- avg_glucose_level ~ gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + stroke + bmi + smoking_status
reg_tree <- reg_spec %>%
  fit(formula = reg_fmla,data = train)
print(reg_tree)

#VISUALIZING THE REGRESSION TREE
reg_tree$fit %>%
  rpart.plot(type = 4, roundint = FALSE)

#GENERATE PREDICTIONS AND COMBINE WITH TEST SET
pred_reg <- predict(reg_tree, new_data = test) %>%
  bind_cols(test)

#OUT-OF-SAMPLE ERROR ESTIMATES FROM yardstick OR ModelMetrics PACKAGE
mae(pred_reg, estimate=.pred, truth=avg_glucose_level) 
rmse(pred_reg, estimate=.pred, truth=avg_glucose_level)

