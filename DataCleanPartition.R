#####DATA EXPLORATION, CLEANING, PARTITIONNG#######

#IMPORTING THE DATA FROM GITHUB REPOSITORY
df<-read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthData.csv")
View(df) #VIEW THE DATA IN SPREADSHEET VIEW

df$gender<-as.factor(df$gender) #CONVERT TO FACTOR
df$hypertension<-as.factor(df$hypertension) #CONVERT TO FACTOR
df$heart_disease<-as.factor(df$heart_disease) #CONVERT TO FACTOR
df$ever_married<-as.factor(df$ever_married) #CONVERT TO FACTOR
df$work_type<-as.factor(df$work_type) #CONVERT TO FACTOR
df$Residence_type<-as.factor(df$Residence_type) #CONVERT TO FACTOR
df$smoking_status<-as.factor(df$smoking_status) #CONVERT TO FACTOR
df$stroke<-as.factor(df$stroke) #CONVERT TO FACTOR

##SOME DESCRIPTIVE ANALYSIS##
summary(df) #SIX NUMBER SUMMARY
hist(df$avg_glucose_level) #HISTOGRAM OF avg_glucose_level
plot(df$avg_glucose_level~df$age) #SCATTER PLOT
cor(df$avg_glucose_level,df$age) #COMPUTE THE CORRELATION

##THEME FROM THE ECONOMIST MAGAZINE
ggplot(df, aes(x = age, y = bmi)) + 
  geom_point() +
  theme_economist()

#Clean the data/outliers that don't make sense
df <- subset(df,age<80)
df <- subset(df, avg_glucose_level>3000)
df <- subset(df, bmi<75)
df <- df[,-1]
View(df)

#Re-examine the plots after cleaning
plot(df$avg_glucose_level~df$age) #SCATTER PLOT

ggplot(df, aes(x = age, y = bmi)) + 
  geom_point() +
  theme_economist()

#Run a linear model
LM<-lm(avg_glucose_level ~ ., df) #BUILD THE MODEL OBJECT USING lm()
summary(LM) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT
LM$coefficients #RETURNS BETA ESTIMATES
LM$residuals #RETURNS RESIDUALS
LM$fitted.values #RETURNS FITTED (PREDICTED) VALUES

#QUESTION:  ARE THE RESIDUALS NORMAL?
hist(LM$residuals) #PLOT THEM!
jarque.bera.test(LM$residuals) #TEST FOR NORMLAITY!

###########################################################
#####PARTITIONING THE DATA INTO TRAINING AND TEST SETS#####
###########################################################

#FRACTION OF DATA TO BE USED AS IN-SAMPLE TRAINING DATA
p<-.7 #70% FOR TRAINING (IN-SAMPLE), 30% FOR TESTING (OUT-OF-SAMPE)

#NUMBER OF OBSERVATIONS IN DATAFRAME
obs_count<-dim(df)[1]

#OF OBSERVATIONS IN THE TRAINING DATA (IN-SAMPLE DATA)
#floor() ROUNDS DOWN TO THE NEAREST WHOLE NUMBER
training_size <- floor(p * obs_count)
training_size

#SET THE RANDOM SEED FOR REPRODUCIBILITY
set.seed(12345)
#RANDOMLY SHUFFLES THE ROW NUMBERS OF ORIGINAL DATASET
train_ind <- sample(obs_count, size = training_size)

Training <- df[train_ind, ] #PULLS RANDOM ROWS FOR TRAINING
Testing <- df[-train_ind, ] #PULLS RANDOM ROWS FOR TESTING

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#SAVE TRAINING AND TESTING DATA
write.csv(Training,"C:\\Users\\pauly\\OneDrive\\Documents\\Education\\USD\\GSBA576 - Analytics\\Project\\HealthDataTrainingClean70.csv",row.names=TRUE)
write.csv(Testing,"C:\\Users\\pauly\\OneDrive\\Documents\\Education\\USD\\GSBA576 - Analytics\\Project\\HealthDataTestingClean30.csv",row.names=TRUE)

