######################################
#HEALTH DATA PREDICTIONS: CART#
######################################

#LOADING THE LIBRARIES
library(tidymodels) #INCLUDES parsnip PACKAGE FOR decision_tree()
library(caret) #FOR confusionMatrix()
library(rpart.plot)

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
#df1$avg_glucose_level<-as.numeric(df1$avg_glucose_level) #CONVERT TO NUMERIC
#df1$bmi<-as.numeric(df1$bmi) #CONVERT TO NUMERIC

summary(df1)#examine the data summary
#####Keep bootstrapping stroke data until it is 50% of data###############
strokes <- factor(c(df1$stroke))
stroke_counts <- table(strokes)
stroke_yes <- "1"
stroke_no <- "0"
count_stroke <- stroke_counts[stroke_yes]
num_stroke <- as.numeric(count_stroke)
count_no_stroke <- stroke_counts[stroke_no]
num_no_stroke <- as.numeric(count_no_stroke)

df_stroke <- df1[df1$stroke == 1,]#select stroke data

while (num_no_stroke > num_stroke){
  df1 <- rbind(df1,df_stroke)  #add more stroke data to increase stroke numbers
  strokes <- factor(c(df1$stroke))
  stroke_counts <- table(strokes)
  stroke_yes <- "1"
  stroke_no <- "0"
  count_stroke <- stroke_counts[stroke_yes]
  num_stroke <- as.numeric(count_stroke)
  count_no_stroke <- stroke_counts[stroke_no]
  num_no_stroke <- as.numeric(count_no_stroke)
  }

##PARTITIONING THE DATA##
set.seed(123)
split<-initial_split(df1, prop=.9, strata=stroke)
train<-training(split)
test<-testing(split)

#SPECIFYING THE CLASSIFICATION TREE MODEL
class_spec <- decision_tree(min_n = 3 , #minimum number of observations for split
                            tree_depth = 30, #max tree depth
                            cost_complexity = 0.01)  %>% #regularization parameter
              set_engine("rpart") %>%
              set_mode("classification")
print(class_spec)

#ESTIMATING THE MODEL (CAN BE DONE IN ONE STEP ABOVE WITH EXTRA %>%)
#gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status
class_tree <- class_spec %>%
  fit(formula = stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi + smoking_status, 
      data = train)
print(class_tree)

#VISUALIZING THE CLASSIFICATION TREE MODEL:
class_tree$fit %>%
  rpart.plot(type = 4, extra = 2, roundint = FALSE)

plotcp(class_tree$fit)

#GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE TEST SET AND COMBINE WITH TEST DATA
pred_class <- predict(class_tree, new_data = test, type="class") %>%
  bind_cols(test) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

pred_prob <- predict(class_tree, new_data = test, type="prob") %>%
  bind_cols(test) #ADD PROBABILITY PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE CONFUSION MATRIX AND DIAGNOSTICS
confusion <- table(pred_class$.pred_class, pred_class$stroke)
confusionMatrix(confusion, positive='1') #FROM CARET PACKAGE

#GENERATE ROC CURVE AND COMPUTE AUC OVER ALL TRUE / FALSE +'s
autoplot(roc_curve(pred_prob, estimate=.pred_1, truth=stroke))
roc_auc(pred_prob, estimate=.pred_1, truth=stroke)

