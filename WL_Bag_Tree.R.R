
#LOADING THE LIBRARIES
library(tidymodels)
library(baguette) #FOR BAGGED TREES
library(xgboost) #FOR GRADIENT BOOSTING
library(caret) #FOR confusionMatrix()
library(vip) #FOR VARIABLE IMPORTANCE
library("magrittr")

#IMPORTING THE DATA
df <- read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTrainingClean70.csv", header=TRUE)

df$gender<-as.factor(df$gender) #CONVERT TO FACTOR
df$hypertension<-as.factor(df$hypertension) #CONVERT TO FACTOR
df$heart_disease<-as.factor(df$heart_disease) #CONVERT TO FACTOR
df$ever_married<-as.factor(df$ever_married) #CONVERT TO FACTOR
df$work_type<-as.factor(df$work_type) #CONVERT TO FACTOR
df$Residence_type<-as.factor(df$Residence_type) #CONVERT TO FACTOR
df$smoking_status<-as.factor(df$smoking_status) #CONVERT TO FACTOR
df$stroke<-as.factor(df$stroke) #CONVERT TO FACTOR


df_stroke <- df[df$stroke == 1,]#select stroke data
df_stroke_weighted <- rbind(df,df_stroke)#combine dataframes to increase stroke numbers
df_stroke_2weighted <- rbind(df_stroke_weighted,df_stroke)
df_stroke_3weighted <- rbind(df_stroke_2weighted,df_stroke)
df_stroke_4weighted <- rbind(df_stroke_3weighted,df_stroke)
df_stroke_5weighted <- rbind(df_stroke_4weighted,df_stroke)
df_stroke_6weighted <- rbind(df_stroke_5weighted,df_stroke)
df_stroke_7weighted <- rbind(df_stroke_6weighted,df_stroke)
df_stroke_8weighted <- rbind(df_stroke_7weighted,df_stroke)
df_stroke_9weighted <- rbind(df_stroke_8weighted,df_stroke)
df_stroke_10weighted <- rbind(df_stroke_9weighted,df_stroke)
df_stroke_11weighted <- rbind(df_stroke_10weighted,df_stroke)
df_stroke_12weighted <- rbind(df_stroke_11weighted,df_stroke)
df_stroke_13weighted <- rbind(df_stroke_12weighted,df_stroke)
df_stroke_14weighted <- rbind(df_stroke_13weighted,df_stroke)
df_stroke_15weighted <- rbind(df_stroke_14weighted,df_stroke)
df_stroke_16weighted <- rbind(df_stroke_15weighted,df_stroke)
df_stroke_17weighted <- rbind(df_stroke_16weighted,df_stroke)
df_stroke_18weighted <- rbind(df_stroke_17weighted,df_stroke)
df_stroke_19weighted <- rbind(df_stroke_18weighted,df_stroke)
df_stroke_20weighted <- rbind(df_stroke_19weighted,df_stroke)
df_stroke_21weighted <- rbind(df_stroke_20weighted,df_stroke)
summary(df_stroke_21weighted)


##PARTITIONING THE DATA##
set.seed(123)
split<-initial_split(df_stroke_21weighted, prop=.7, strata=stroke)
train<-training(split)
test<-testing(split)

#MODEL DESCRIPTION:
fmla <- stroke ~.

##############################
#SPECIFYING BAGGED TREE MODEL#
##############################

spec_bagged <- bag_tree(min_n = 20 , #minimum number of observations for split
                        tree_depth = 30, #max tree depth
                        cost_complexity = 0.01, #regularization parameter
                        class_cost = NULL)  %>% #for output class imbalance adjustment (binary data only)
               set_mode("classification") %>% #can set to regression for numeric prediction
               set_engine("rpart", times=100) #times = # OF ENSEMBLE MEMBERS IN FOREST
print(spec_bagged)
#FITTING THE MODEL
set.seed(123)
bagged_forest <- spec_bagged %>%
  fit(formula = fmla, data = df_stroke_21weighted)
print(bagged_forest)

#GENERATE IN-SAMPLE PREDICTIONS ON THE TRAIN SET AND COMBINE WITH TRAIN DATA
pred_class_bf_in <- predict(bagged_forest, new_data = train, type="class") %>%
  bind_cols(train) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE IN-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
confusion <- table(pred_class_bf_in$.pred_class, pred_class_bf_in$stroke)
confusionMatrix(confusion) #FROM CARET PACKAGE

#GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE TEST SET AND COMBINE WITH TEST DATA
pred_class_bf_out <- predict(bagged_forest, new_data = test, type="class") %>%
  bind_cols(test) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE OUT-OF-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
confusion <- table(pred_class_bf_out$.pred_class, pred_class_bf_out$stroke)
confusionMatrix(confusion) #FROM CARET PACKAGE

################################
#RUNNING 15% OF TRAINING DATA THROUGH BAG TREES MODEL#
################################
Imported data from HealthDataTesting15 files using the builtin import function in r studio. labled dftest
                    
dftest$gender<-as.factor(dftest$gender) #CONVERT TO FACTOR
dftest$hypertension<-as.factor(dftest$hypertension) #CONVERT TO FACTOR
dftest$heart_disease<-as.factor(dftest$heart_disease) #CONVERT TO FACTOR
dftest$ever_married<-as.factor(dftest$ever_married) #CONVERT TO FACTOR
dftest$work_type<-as.factor(dftest$work_type) #CONVERT TO FACTOR
dftest$Residence_type<-as.factor(dftest$Residence_type) #CONVERT TO FACTOR
dftest$smoking_status<-as.factor(dftest$smoking_status) #CONVERT TO FACTOR
dftest$stroke<-as.factor(dftest$stroke) #CONVERT TO FACTOR


#GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE 15% TEST SET 
predtest_class_bf_out <- predict(bagged_forest, new_data = dftest, type="class") %>%
  bind_cols(dftest) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE OUT-OF-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
confusion <- table(predtest_class_bf_out$.pred_class, predtest_class_bf_out$stroke)
confusionMatrix(confusion) #FROM CARET PACKAGE
                    