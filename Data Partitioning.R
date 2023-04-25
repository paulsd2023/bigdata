###########################################################
#####AN EXAMPLE OF VALIDATION WITH SOME MARKETING DATA#####
###########################################################
#IMPORTING THE DATA FROM GITHUB REPOSITORY
df<-read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthData.csv")
#class(df$avg_glucose_level)="Numeric"
#class(df$age)="Numeric"
View(df) #VIEW THE DATA IN SPREADSHEET VIEW

##SOME DESCRIPTIVE ANALYSIS##
summary(df) #SIX NUMBER SUMMARY
hist(df$avg_glucose_level) #HISTOGRAM OF avg_glucose_level
plot(df$avg_glucose_level~df$age) #SCATTER PLOT
cor(df$avg_glucose_level,df$age) #COMPUTE THE CORRELATION

#ADDING NONLINEAR (POLYNOMIAL) FEATURE TRANSFORMATIONS OF ad_time
df$ad_time2<-df$ad_time^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
df$ad_time3<-df$ad_time^3 #CUBIC TRANSFORMATION (3rd ORDER)
df$ad_time4<-df$ad_time^4 #FOURTH ORDER TERM

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
write.csv(Training,"C:\\Users\\pauly\\OneDrive\\Documents\\Education\\USD\\GSBA576 - Analytics\\Project\\HealthDataTraining70.csv",row.names=TRUE)
write.csv(Testing,"C:\\Users\\pauly\\OneDrive\\Documents\\Education\\USD\\GSBA576 - Analytics\\Project\\HealthDataTesting30.csv",row.names=TRUE)

