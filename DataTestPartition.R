#####DATA EXPLORATION, CLEANING, PARTITIONNG#######

#IMPORTING THE DATA FROM GITHUB REPOSITORY
df<-read.csv("https://raw.githubusercontent.com/paulsd2023/bigdata/main/HealthDataTestingClean30.csv")
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

#####PARTITIONING THE DATA INTO TRAINING AND TEST SETS#####
###########################################################

#FRACTION OF DATA TO BE USED AS TWO SETS OF TEST DATA
p<-.5 #50% FOR TRAINING (IN-SAMPLE), 30% FOR TESTING (OUT-OF-SAMPE)

#NUMBER OF OBSERVATIONS IN DATAFRAME
obs_count<-dim(df)[1]

#OF OBSERVATIONS IN THE TRAINING DATA (IN-SAMPLE DATA)
#floor() ROUNDS DOWN TO THE NEAREST WHOLE NUMBER
testing_size <- floor(p * obs_count)
testing_size

#SET THE RANDOM SEED FOR REPRODUCIBILITY
set.seed(12345)
#RANDOMLY SHUFFLES THE ROW NUMBERS OF ORIGINAL DATASET
test_ind <- sample(obs_count, size = testing_size)

TestingInitial <- df[test_ind, ] #PULLS RANDOM ROWS FOR TRAINING
TestingFinal <- df[-test_ind, ] #PULLS RANDOM ROWS FOR TESTING

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(TestingInitial)
dim(TestingFinal)

#SAVE TRAINING AND TESTING DATA
write.csv(TestingInitial,"C:\\Users\\pauly\\OneDrive\\Documents\\Education\\USD\\GSBA576 - Analytics\\Project\\HealthDataTestigInitial15.csv",row.names=TRUE)
write.csv(TestingFinal,"C:\\Users\\pauly\\OneDrive\\Documents\\Education\\USD\\GSBA576 - Analytics\\Project\\HealthDataTestingFinal15.csv",row.names=TRUE)

