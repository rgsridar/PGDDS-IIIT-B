
# Set the path and librarys
setwd("C:/Users/vidhy/Desktop/UpGrad/Elective_HC")


library("MASS")
library("icd")
library("car")   
library("caret") 
library("ggplot2")
library("cowplot")
library("caTools")  
library("e1071")
library("tidyr")


# Creating Dia_DF and reading the CSV

Dia_DF <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE)
str(Dia_DF)
head(Dia_DF) # Check

#I have addressed  the Data preparation and missing value treatments differntly while modeling Random Forest. tryied two different approches.
#Data preparation

#Q1 :REMOVE REDUNDANT VARIABLES 

duplicated(colnames(Dia_DF))
length(unique(tolower(Dia_DF$encounter_id)))  
# 101766 count comes same as total number of rows hence no ducplicate.Confirming encounter_id is primary key 
nrow(Dia_DF) == length(unique(tolower(Dia_DF$encounter_id)))
# TRUE

#Q2 :Check for missing values and treat them accordingly.
# There are ? where we will convert to "NA"
Dia_DF <- replace(Dia_DF, Dia_DF =="?", NA)
sapply(X = Dia_DF, FUN = function(x) sum(is.na(x)))
View(Dia_DF)
# 1 Weight has lot of NAs and we can remove/ignore it
# 2 Race is of no use for our analysis and we can remove
# 3 Payer code,Medical Speciality is not required for our analysis and we can remove/ignore them
# Treatments for the same done in subsiquent codes below after Graphs and data explorations 

# Q3 done along with Q5 for easiyness and graphs  
#Q4 :Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".

Dia_DF$readmitted <- ifelse(Dia_DF$readmitted %in% c("<30",">30"),'yes','no')

Dia_DF$readmitted <- factor(Dia_DF$readmitted)
levels(Dia_DF$readmitted)<-c(1,0)
Dia_DF$readmitted<- as.numeric(levels(Dia_DF$readmitted))[Dia_DF$readmitted]

View(Dia_DF)
colnames(Dia_DF)


# Q3 Scale numeric attributes and create dummy variables for categorical ones. 
# Q5 Create the derived metric 'comorbidity', according to the following scheme (as given in problem statement)

Dia_DF$gender <- factor(Dia_DF$gender)
levels(Dia_DF$gender)<-c(1,0)
Dia_DF$gender<- as.numeric(levels(Dia_DF$gender))[Dia_DF$gender]



Dia_DF$change <- factor(Dia_DF$change)
levels(Dia_DF$change)<-c(1,0)
Dia_DF$change<- as.numeric(levels(Dia_DF$change))[Dia_DF$change]



Dia_DF$diabetesMed <- factor(Dia_DF$diabetesMed)
levels(Dia_DF$diabetesMed)<-c(1,0)
Dia_DF$diabetesMed<- as.numeric(levels(Dia_DF$diabetesMed))[Dia_DF$diabetesMed]

# colnames(Dia_DF)

dummy_1 <- data.frame(model.matrix( ~race, data = Dia_DF))
dummy_1 <- dummy_1[,-1]
#Dia_DF <- cbind(Dia_DF[,-3], dummy_1)
Dia_DF <- cbind(Dia_DF[,], dummy_1)
# colnames(Dia_DF)


dummy_2 <- data.frame(model.matrix( ~age, data = Dia_DF))
dummy_2 <- dummy_2[,-1]
Dia_DF <- cbind(Dia_DF[,], dummy_2)

dummy_3 <- data.frame(model.matrix( ~insulin, data = Dia_DF))
dummy_3 <- dummy_3[,-1]
Dia_DF <- cbind(Dia_DF[,], dummy_3)


str(Dia_DF)

Dia_DF$diag_1 <- as.character(Dia_DF$diag_1)
Dia_DF$diag_2 <- as.character(Dia_DF$diag_2)
Dia_DF$diag_3 <- as.character(Dia_DF$diag_3)

Dia_DF$Comm1 <-
  1*(
    1*((as.numeric(Dia_DF$diag_1) > 250) &  (as.numeric(Dia_DF$diag_1) < 251)) 
    |
      1*((as.numeric(Dia_DF$diag_2) > 250) &  (as.numeric(Dia_DF$diag_2) < 251))
    |
      1*((as.numeric(Dia_DF$diag_3) > 250) &  (as.numeric(Dia_DF$diag_3) < 251))
  )

Dia_DF$Comm1[is.na(Dia_DF$Comm1)] <- 0


Dia_DF$Comm2 <-
  1*(
    1*((as.numeric(Dia_DF$diag_1) > 389) &  (as.numeric(Dia_DF$diag_1) < 460)) 
    |
      1*((as.numeric(Dia_DF$diag_2) > 389) &  (as.numeric(Dia_DF$diag_2) < 460))
    |
      1*((as.numeric(Dia_DF$diag_3) > 389) &  (as.numeric(Dia_DF$diag_3) < 460))
  )

Dia_DF$Comm2[is.na(Dia_DF$Comm2)] <- 0

View(Dia_DF)

# Comorbidity Matrix

Dia_DF$comorbidity <- ifelse(Dia_DF$Comm1 ==0 & Dia_DF$Comm2 == 0, 0, 
                             ifelse(Dia_DF$Comm1 == 1 & Dia_DF$Comm2 == 0, 1, 
                                    ifelse(Dia_DF$Comm1 ==0 & Dia_DF$Comm2 == 1, 2, 3))
)


#As we created a seperate Comorbidity matrix , we dont need columns diag_1, diag_2, diag_3, Comm1 & Comm2
Dia_DF <- Dia_DF[,-which(names(Dia_DF) == "diag_1")]
Dia_DF <- Dia_DF[,-which(names(Dia_DF) == "diag_2")]
Dia_DF <- Dia_DF[,-which(names(Dia_DF) == "diag_3")]
Dia_DF <- Dia_DF[,-which(names(Dia_DF) == "Comm1")]
Dia_DF <- Dia_DF[,-which(names(Dia_DF) == "Comm2")]
View(Dia_DF)



#Data Exploration

#Perform basic data exploration for some categorical attributes
#Perform basic data exploration for some numerical attributes


#Finding the Diabetes patients in each Comorbidity catrogory across all patients
ggplot(data = Dia_DF, aes(x = comorbidity )) + geom_bar(fill = 'Yellow')

# Finding age wise readminssion data
ggplot(data = Dia_DF,aes(age,fill = readmitted)) + geom_bar(fill = " Red") + 
  ggtitle("Readmission Data - Age wise")

# Finding out how many are taking the diabetes medicines

ggplot(data = Dia_DF, aes(x = diabetesMed)) + geom_bar(fill = 'green')

# Readmission due to Insulin
ggplot(data = Dia_DF,aes(insulin,fill = readmitted)) + geom_bar(fill = "blue") + 
  ggtitle("Readmission - Insulin")


# Bi-Variate analysis of males and females are suffering from diabetes using Comorbidity

ggplot(data = Dia_DF,aes(comorbidity,fill = gender)) + geom_bar(fill = "blue") + 
  ggtitle("Population split across Comorbidity")

## Max glucose serum

ggplot(data = Dia_DF, aes(x = max_glu_serum)) + geom_bar(fill = 'blue')


View(Dia_DF)
# We will remove Unwanted and NAs before we go to Model builing (Eg) Removing weight, payer_code and medical_speciality columns, as they are of not much help. 

Dia_DF <- Dia_DF[, -c(4,6 ,11, 12)]

# We dont need Race as well
Dia_DF <- Dia_DF[, -3]
# We dont need age as we have already splitted the age 
Dia_DF <- Dia_DF[, -3]



dummy_1 <- data.frame(model.matrix( ~max_glu_serum, data = Dia_DF))
dummy_1 <- dummy_1[,-1]
Dia_DF <- cbind(Dia_DF[,-14], dummy_1)

dummy_2 <- data.frame(model.matrix( ~A1Cresult, data = Dia_DF))
dummy_2 <- dummy_2[,-1]
Dia_DF <- cbind(Dia_DF[,-14], dummy_2)

View(Dia_DF)



#As we will do risk stratification only with insulin we can remove other drugs

Dia_DF <- Dia_DF[, -c(14:30, 32:36)]



########Model building Logistic Regression#############################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
set.seed(100)
train <- which(sample.split(Dia_DF$readmitted,SplitRatio = 0.7))
test <- which(!c(1:nrow(Dia_DF) %in% train))
Dia_Train <- Dia_DF[train,]
Dia_Test <- Dia_DF[test,]

# First Model using BiNomial
M1 = glm(readmitted ~ ., data = Dia_Train, family="binomial")
summary(M1) #AIC: 91595



M2 <- glm(readmitted ~ patient_nbr + admission_type_id + discharge_disposition_id + 
                 admission_source_id + time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + diabetesMed + age.10.20. + age.30.40. + age.40.50. + age.50.60. + 
                 age.60.70. + age.70.80. + age.80.90. + insulinNo + insulinSteady + 
                 insulinUp + comorbidity,
               data=Dia_Train, family="binomial")

summary(M2) #AIC: 92615


# We will do  Step wise selection
M3 = stepAIC(M2,direction = "both")
summary(M3)

vif(M3)

#Assumption: Based on above We will remove few columns and check for others which has direct impact on Risk. 
# Note : Due to time constraint I have not removed one by one and created mulptiple models to check values.
# Assumption we will remove the less relative parameters which are more likely have a relation with readmission.

M4 <- glm(readmitted ~ patient_nbr  + admission_type_id + discharge_disposition_id + 
                 admission_source_id + time_in_hospital + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + diabetesMed + insulinSteady + 
                 comorbidity,
               data=Dia_Train, family="binomial")

summary(M4)# IC 92689

vif(M4)

# Z value is lesser and hence based on above results we can consider M4 as final
View(M4)

#################Model Evaluation  - Logistic Regression#########################


# Lets see what are the predictions made
Dia_Predicted <- predict(M4, type="response", newdata = dplyr::select(Dia_Test, -readmitted))
Dia_Test$Readmitted_Predicted <- Dia_Predicted
View(Dia_Test)


# Let's use the probability cutoff of 50%.
Dia_Test_Pred <- factor(ifelse(Dia_Predicted >= 0.50, "Yes", "No"))
Dia_Test_Actual <- factor(ifelse(Dia_Test$readmitted == 1,"Yes","No"))
table(Dia_Test_Actual, Dia_Test_Pred)
#Dia_Test_Pred
#Dia_Test_Actual    No   Yes
#No   5780  8291
#Yes  3349 13110


# Confution Matrix
Conf_Test <- confusionMatrix(Dia_Test_Pred, Dia_Test_Actual, positive = "Yes")
Conf_Test

#Confusion Matrix and Statistics

#Reference
#Prediction    No   Yes
#No   5780  3349
#Yes  8291 13110
#Accuracy : 0.6187          
#95% CI : (0.6133, 0.6242)
#No Information Rate : 0.5391          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.2127          
#Mcnemar's Test P-Value : < 2.2e-16       
#Sensitivity : 0.7965          
#Specificity : 0.4108          
#Pos Pred Value : 0.6126          
#Neg Pred Value : 0.6331          
#Prevalence : 0.5391          
#Detection Rate : 0.4294          
#Detection Prevalence : 0.7010          
#Balanced Accuracy : 0.6036          
#'Positive' Class : Y
# 'Positive' Class : Yes             


## Find out the optimal probalility cutoff 

Cutoff <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(Dia_Predicted >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, Dia_Test_Actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(Dia_Predicted)

# Creating cutoff values from 0003413 to 0.8152061 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.81,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = Cutoff(s[i])
} 

cutoff <- s[which(abs(OUT[,1]-OUT[,2]) < 0.6)]
cutoff  

#Lets view the ROC Curve 
plot(s, OUT[,1],xlab="CUTOFF",ylab="VALUES",cex.lab=2,cex.axis=2,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=2)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=2)
lines(s,OUT[,2],col="Yellow",lwd=3)
lines(s,OUT[,3],col=4,lwd=3)
# Note : Legend Red Sensitivity ; Yellow Specificity and Blue Accuracy
#box()
#legend(0.5,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"),cex = 0.75)

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.6)]
cutoff <- mean(cutoff) 
View(cutoff)## 0.5433333


# We will choose the cutoff value = 0.5433333 at the point where sensitivity & specificity are nearest to eachother
Final_test_Cutoff<- factor(ifelse(Dia_Predicted >=0.5433333, "Yes", "No"))

conf_final <- confusionMatrix(Final_test_Cutoff, Dia_Test_Actual, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec
#Accuracy :: 62.07%
#Sensitivity :: 68.5%
#Specificity ::54.56%


### KS -statistic - Test Data ######

Final_test_Cutoff <- ifelse(Final_test_Cutoff=="Yes",1,0)
Dia_Test_Actual <- ifelse(Dia_Test_Actual=="Yes",1,0)

install.packages("ROCR")
library("ROCR")
#on testing  data
pred_object_test<- prediction(Final_test_Cutoff, Dia_Test_Actual)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

AUC<-performance(pred_object_test,measure='auc')
AUC<-round(as.numeric(AUC@y.values[[1]]),1)
AUC
#Area under the curve: 0.6 which is expected from a good model.
par(mfrow=c(1,1))
plot(performance_measures_test, main = "ROC Curve :  Area under the curve 0.6")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) 
# KS- statistic is a measure of how well our model is able to distinguish between the Good and the Bad categories.




################################### Model Buliding - Radom Forest#############################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# I have created a seperate data frame for Random forest and did few more analysis and not used the Dia_DF
# Creating Dia_RF (for Random forest evaluation) and reading the CSV

Dia_RF <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE)
str(Dia_RF)
head(Dia_RF) # Check

#Data preparation

# Removing the redudant variables 

duplicated(colnames(Dia_RF))
length(unique(tolower(Dia_RF$encounter_id)))  
# 101766 count comes same as total number of rows hence no ducplicate.Confirming encounter_id is primary key 
nrow(Dia_RF) == length(unique(tolower(Dia_RF$encounter_id)))
#unique values for each column
as.data.frame(rapply(Dia_RF,function(x)length(unique(x))))
#On Rows
names(Dia_RF[sapply(Dia_RF, function(x) length(unique(x))==1)])  # "examide" & "citoglipton"
names(Dia_RF[sapply(Dia_RF, function(x) length(unique(x))==nrow(Dia_RF))])  # "encounter_id"
# Lets remove all unique values for "enconter_id"
Dia_RF <- Dia_RF[sapply(Dia_RF, function(x) length(unique(x))!=nrow(Dia_RF))]

#Check for missing values and treat them accordingly.
# Lets remove the colums which are not related to this analysis.
# We will also remove all the medicines as they are not relavent for our risk analysis
# Lets keep Insulin for our diabetic analysis.


Dia_RF <- Dia_RF[!names(Dia_RF) %in% c("patient_nbr","admission_type_id", "max_glu_serum", "metformin", "repaglinide", 
                                       "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide","glipizide",
                                       "glyburide","tolbutamide", "pioglitazone", "rosiglitazone","acarbose","miglitol", 
                                       "troglitazone","tolazamide","glyburide.metformin", "glipizide.metformin",
                                       "glimepiride.pioglitazone","metformin.rosiglitazone", "metformin.pioglitazone")]
str(Dia_RF)

sum(duplicated(Dia_RF)) # There is no duplicated rows

View(Dia_RF)
# While viewing Dia_RF we can find "?" in the data frame.
# We can also see Payer_code,Weight,medical_specialty are having high missing values 

Dia_RF <- Dia_RF[!names(Dia_RF) %in% c("payer_code", "weight", "medical_specialty")]

#We can drop the race as its not important for our analysis
Dia_RF <- Dia_RF[, -1]

#Dropped invalid entries and ? in Diag 1,2,3
Dia_RF <- Dia_RF[Dia_RF$diag_1 != "?",]
Dia_RF <- Dia_RF[Dia_RF$diag_2 != "?",]
Dia_RF <- Dia_RF[Dia_RF$diag_3 != "?",]
#Check for Missing values
sum(is.na(Dia_RF))   # 0 NA values

#Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".

Dia_RF$readmitted <- ifelse(Dia_RF$readmitted %in% c("<30",">30"),'yes','no')

Dia_RF$readmitted <- factor(Dia_RF$readmitted)
levels(Dia_RF$readmitted)<-c(1,0)
Dia_RF$readmitted<- as.numeric(levels(Dia_RF$readmitted))[Dia_RF$readmitted]

View(Dia_RF)
colnames(Dia_RF)

#Create the derived metric 'comorbidity', according to the following scheme (as given in problem statement)

Dia_RF$gender <- factor(Dia_RF$gender)
levels(Dia_RF$gender)<-c(1,0)
Dia_RF$gender<- as.numeric(levels(Dia_RF$gender))[Dia_RF$gender]

Dia_RF$change <- factor(Dia_RF$change)
levels(Dia_RF$change)<-c(1,0)
Dia_RF$change<- as.numeric(levels(Dia_RF$change))[Dia_RF$change]

Dia_RF$diabetesMed <- factor(Dia_RF$diabetesMed)
levels(Dia_RF$diabetesMed)<-c(1,0)
Dia_RF$diabetesMed<- as.numeric(levels(Dia_RF$diabetesMed))[Dia_RF$diabetesMed]

dummy_1 <- data.frame(model.matrix( ~race, data = Dia_RF))
dummy_1 <- dummy_1[,-1]

Dia_RF <- cbind(Dia_RF[,], dummy_1)

dummy_2 <- data.frame(model.matrix( ~age, data = Dia_RF))
dummy_2 <- dummy_2[,-1]

Dia_RF <- cbind(Dia_RF[,], dummy_2)

dummy_3 <- data.frame(model.matrix( ~insulin, data = Dia_RF))
dummy_3 <- dummy_3[,-1]

Dia_RF <- cbind(Dia_RF[,], dummy_3)

str(Dia_RF)

Dia_RF$diag_1 <- as.character(Dia_RF$diag_1)
Dia_RF$diag_2 <- as.character(Dia_RF$diag_2)
Dia_RF$diag_3 <- as.character(Dia_RF$diag_3)

Dia_RF$Comm1 <-
  1*(
    1*((as.numeric(Dia_RF$diag_1) > 250) &  (as.numeric(Dia_RF$diag_1) < 251)) 
    |
      1*((as.numeric(Dia_RF$diag_2) > 250) &  (as.numeric(Dia_RF$diag_2) < 251))
    |
      1*((as.numeric(Dia_RF$diag_3) > 250) &  (as.numeric(Dia_RF$diag_3) < 251))
  )

Dia_RF$Comm1[is.na(Dia_RF$Comm1)] <- 0


Dia_RF$Comm2 <-
  1*(
    1*((as.numeric(Dia_RF$diag_1) > 389) &  (as.numeric(Dia_RF$diag_1) < 460)) 
    |
      1*((as.numeric(Dia_RF$diag_2) > 389) &  (as.numeric(Dia_RF$diag_2) < 460))
    |
      1*((as.numeric(Dia_RF$diag_3) > 389) &  (as.numeric(Dia_RF$diag_3) < 460))
  )

Dia_RF$Comm2[is.na(Dia_RF$Comm2)] <- 0

View(Dia_RF)

# Comorbidity Matrix

Dia_RF$comorbidity <- ifelse(Dia_RF$Comm1 ==0 & Dia_RF$Comm2 == 0, 0, 
                             ifelse(Dia_RF$Comm1 == 1 & Dia_RF$Comm2 == 0, 1, 
                                    ifelse(Dia_RF$Comm1 ==0 & Dia_RF$Comm2 == 1, 2, 3))
)

#As we created a seperate Comorbidity matrix , we dont need columns diag_1, diag_2, diag_3, Comm1 & Comm2
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "diag_1")]
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "diag_2")]
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "diag_3")]
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "Comm1")]
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "Comm2")]
#View(Dia_RF)

#Convert to Factor
#Dia_RF <- cbind(Dia_RF[c(6:12,16)], lapply(Dia_RF[c(1:5,13:15,17:22)],factor))

#Convert data type to Numeric 
Dia_RF$time_in_hospital <- as.numeric(Dia_RF$time_in_hospital);
Dia_RF$num_lab_procedures <- as.numeric(Dia_RF$num_lab_procedures);
Dia_RF$num_procedures <- as.numeric(Dia_RF$num_procedures);
Dia_RF$num_medications <- as.numeric(Dia_RF$num_medications);
Dia_RF$number_outpatient <- as.numeric(Dia_RF$number_outpatient);
Dia_RF$number_emergency <- as.numeric(Dia_RF$number_emergency);
Dia_RF$number_inpatient <- as.numeric(Dia_RF$number_inpatient);
Dia_RF$number_diagnoses <- as.numeric(Dia_RF$number_diagnoses);
Dia_RF$time_in_hospital <- scale(Dia_RF$time_in_hospital) 
Dia_RF$num_lab_procedures <- scale(Dia_RF$num_lab_procedures) 
Dia_RF$num_procedures <- scale(Dia_RF$num_procedures)
Dia_RF$num_medications <- scale(Dia_RF$num_medications) 
Dia_RF$number_outpatient <- scale(Dia_RF$number_outpatient) 
Dia_RF$number_emergency <- scale(Dia_RF$number_emergency) 
Dia_RF$number_inpatient <- scale(Dia_RF$number_inpatient) 
Dia_RF$number_diagnoses <- scale(Dia_RF$number_diagnoses) 

#View(Dia_RF)

# We will remove gender
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "gender")]
# We will also remove age as we have splitted
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "age")]
# Remove examide and Citoglipton
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "examide")]
Dia_RF <- Dia_RF[,-which(names(Dia_RF) == "citoglipton")]

# Checking Attrition rate of Employee
Readmitted <- sum(Dia_RF$readmitted)/nrow(Dia_RF)
Readmitted 
# We can see 53.69% is the readmission rate

#Converting other factor variable to dummy

dummys <- which(colnames(Dia_RF) %in% 
                  c('discharge_disposition_id','admission_source_id', 
                    'A1Cresult', 'insulin', 'comorbidity'))

diabetic_chr <- Dia_RF[, dummys]

diabetic_chr<- data.frame(sapply(diabetic_chr, function(x) if (is.factor(x)) x else factor(x) ))

str(diabetic_chr)

#creating dummy variables for factor attributes
dummies <- data.frame(sapply(diabetic_chr, 
                             function(x) data.frame(model.matrix(~x-1,data = diabetic_chr))[,-1]))

#Data set for our analysis
final <- cbind(Dia_RF[,-c(dummys)],dummies)

str(final) # 100244 obs of 73variables


# We will split 70% and 30%

set.seed(100)

ind = sample.split(final$readmitted, SplitRatio = 0.7)  

train = final[ind,] 

test = final[!(ind),] 


### Model Buliding - Radom Forest#########

library(randomForest)

set.seed(71)

#Making our target class to factor
train$readmitted <- factor(train$readmitted)
test$readmitted <- factor(test$readmitted)


#3-fold cross validation to find optimal mtry value
trControl <- trainControl(method = "cv",
                          number = 3,
                          search = "grid",
                          verboseIter = TRUE)

tuneGrid <- expand.grid(.mtry = c(1: 10))

# The below code will take more time .. Approx 30 Mins 

rf_mtry <- train(readmitted ~ .,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 100,
                 verbose = TRUE)

#Fitting mtry =5


# Below step will take time as we considered ntree as 1000. Approx 20 mins. ntree can be reduced for lesser time.

model_rf <- randomForest(readmitted ~ ., data=train, proximity=FALSE,
                         ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)

model_rf

# Prediction

pred <- predict(model_rf, newdata=test)

table(pred, test$readmitted)

Conf_RF <- confusionMatrix(pred, test$readmitted)

accuracy <- Conf_RF$overall[1]

accuracy  #63.8%

# Model comparison***************************************** 
# Logical regression Accuracy is 62.07% and Random forest is 63.8%. Random forst has performed better marginally.

### Risk Stratification #########
#Stratify your population into 3 risk buckets:
#High risk (Probability of readmission >0.7)
#Medium risk (0.3 < Probability of readmission < 0.7)
#Low risk (Probability of readmission < 0.3)

readmin_prob <- predict(model_rf, newdata=final, type="prob")

final$readmin_prob <- readmin_prob[,2]

final$RiskBucket <- ifelse(round(final$readmin_prob,2) > 0.70, "High Risk",
                           ifelse(round(final$readmin_prob,2) >= 0.30 & round(final$readmin_prob,2) <= 0.70, "Medium Risk",
                                  ifelse(round(final$readmin_prob,2) < 0.30, "Low Risk", "")))

table(as.factor(final$RiskBucket))

View(final)

#High risk 
High_Risk <- sum(final$RiskBucket == "High Risk")/nrow(final)
High_Risk # 41.5% population has high risk of readmission


#Medium risk 
Medium_Risk <- sum(final$RiskBucket == "Medium Risk")/nrow(final)
Medium_Risk # 40.4% population has medium risk of readmission


#Low risk 
Low_Risk <- sum(final$RiskBucket == "Low Risk")/nrow(final)
Low_Risk # 17.9% population has low risk of readmission


####################################END #################################################




