

##GANESH SRIDAR  - SVM MODELING  21/09/2018
#####################################################################################################
# Key assumptions
#1.  As informed I will be using 15% of the data to reduce the time for execution.It is assumed 15% covers all the variants and styles encoded in handwritten digits.


#We shall follow the approch using CRISP-DM methology
# a. Technical and Business aspects 
##  Problemm in field of pattern recognition of handwritten digit recognition
##   Our goal is to develop a model that can correctly identify the digit (between 0-9) written in an image based on 
###  Pixel values given as features.


# b. Data Understanding
# Data is collected from mnist database (mnist_training and mnist test)
## Data set has 60,000 data points and 785 Variables in training and 10,000 data points and 785 Variables in test
###We have range of data between 0 and 255 and  class (0-9)
#### It is observed data set having no lables/headers, updated the Coloum Name as "Numbers"
#### No Missing Values in mnist data

# c. Data Preparation
#### We will merge test and train data using using rbind() function to MergedDB
#### after Merge We will remove all predictors with nvz parameter equal to TRUE
### we will use nearZeroVar() in Caret package and remove all true
### We will use Indexing and  Spliting to Test and Training using Index (INX) created
#### segrate the the for training and test by Sampling 15% (Assumption)

# d. Model Building

#Evaluate Linear SVM Model
##Evaluate Polynomial SVM Model
### SVM RBF Model using ksvm
#### Hyperparameter tuning using three fold is used for C=1,2,3 and Sigma 2.215e-3, 3.215e-3, 4.215e-3
##### I have dones cross validation using 3 fold, We can also use 5 fold etc.. To save time I have used 3 fold.
###### Finalize based on final parameters 
# e. Model Evaluation
#Using the Confusion matrix to evaluate the model Accuracy 
#The final values used for the model were sigma = 0.004215 and C = 3.
# f. Model Deployment
### Our  final model will be deployed to testdata

####################################################################################################


# Data Understanding 
# Setting Working Directory 

setwd("C:/Users/gr33/Desktop/UpGrad/COURSE4/SVM/My Solution")

#Required Library's
required_packages <- c("kernlab","readr","caret","dplyr","caTools","ggplot2","gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
      
library(kernlab)
library(caret)
library(caTools)
library(ggplot2)
library(readr)
library(dplyr)
library(stats)

# Optional Parallel package R can be divided into implicit and explicit computing mode.
install.packages("doParallel")
library(doParallel) # for R Parallel process.


# Optional based on error.
#Some time COnfusion matrix error comes that e1071 package to be loased, Hence if required install the below packages
install.packages("e1071")
library(e1071)


#Loading data in to dataset
training_database<- read.csv("mnist_train.csv", header = FALSE)
testing_database<- read.csv("mnist_test.csv", header = FALSE)
      
# Lets check Training Dataset

Ploting1 <- ggplot(training_database) + 
  geom_bar(aes(x=as.factor(Numbers)),fill="Yellow",col="Red") +
  scale_y_continuous(breaks = seq(0,4700,500)) +
  labs(title="Distribution",x="Numbers",y="Count")

Ploting1  # All the Numbers are 0-9 

## Data Preparation

## Check -  for missing value imputation, removing duplicate data and other kinds of data redundancies 

#Since datasets does not have column names and column refer to numbers,We will rename the first column of both datasets as "Numbers"
colnames(training_database)[1]<- "Numbers"
colnames(testing_database)[1]<- "Numbers"


# Checking records and attributes of training and test database
dim(training_database) # 60000 records with 785 attributes
dim(testing_database) # 10000 records with 785 attributes


head(training_database) #  28x28 pixel information for the digit 5.

# We need to check the database are consistant 
str(training_database)
training_database[, lapply(training_database[,-1], is.numeric) == FALSE]  # All are integer datatype
str(testing_database)
testing_database[, lapply(testing_database[,-1], is.numeric) == FALSE]  # All are integer datatype
      
#CHECKs 
sapply(training_database[,-1], min)
min(sapply(training_database[,-1], min)) # Min value 0
sapply(training_database[,-1], max)
max(sapply(training_database[,-1], max)) # Max is 255
# Hence Range is 0 to 255

# Check for any missing or NA values.
sapply(training_database, function(x) sum(is.na(x)))
max(sapply(training_database, function(x) sum(is.na(x)))) # No missing values 
sapply(testing_database, function(x) sum(is.na(x)))
max(sapply(testing_database, function(x) sum(is.na(x))))# No NA s
      


# Modeling and evaluation
# Lets combine the data base in MergedDB dataset and later we will split using Index "IDX"
INX<- nrow(training_database)
MergedDB<- rbind(training_database, testing_database)
# We will check for any Zero variability or columns with only one unique value and we will remove it any.
MergedDB<- MergedDB[vapply(MergedDB, function(datachop) length(unique(datachop))>1, logical(1L))] 
      
#Will use  nearZeroVar() by using nearZeroVar in Caret package
 nzv_database<- nearZeroVar(MergedDB, saveMetrics = TRUE)
 range(nzv_database$percentUnique) # It is 28.5% to 36.5% which is low.
 head(nzv_database)
      
# We will remove all predictors with nvz parameter equal to TRUE
 
dim(nzv_database[nzv_database$nzv==FALSE,])
MergedDB<- MergedDB[c(rownames(nzv_database[nzv_database$nzv==FALSE, ]))]
 


# Spliting to Test and Training using INX created earlier.
training_database<- MergedDB[1:INX,]
test_data<- MergedDB[(INX+1):nrow(MergedDB), ]
#Convert to Factor type
test_data$Numbers<- factor(test_data$Numbers)
training_database$Numbers<- factor(training_database$Numbers)
      

#Sampling we will use 15% as per information received from Upgrad to reduce the time .
#We will assume this 15% will cover all variants and styles of encoded handwriiten digits.
# Sample Split is used to take 15% sample.
set.seed(100)
train_sample_index <- sample.split(training_database$Numbers,SplitRatio = 0.15)
Sample_Training_data <- training_database[train_sample_index,]

#Lets check the Sampled data : 

Plotting2 <- ggplot(Sample_Training_data) + 
  geom_bar(aes(x=as.factor(Numbers)),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,4700,500)) +
  labs(title="Sampled Training",x="Sampled Numbers",y="Counts")
Plotting2 # The distribution looks fine
      

## OPTIONAL :Parallel computing technology can solve the problem that single-core and memory capacity can not meet the application needs.


X = makeCluster(detectCores())
registerDoParallel(X)




# MODEL BUILDING : Linear SVM Model with default parameters
SVM_Linear <- ksvm(Numbers~.,
data=Sample_Training_data,
scale=FALSE,
kernel="vanilladot")
SVM_Linear
#Default Parameters Penalty for Misclassification Cost [C=1]
#Number of support vectors influencing decision boundary = 2523
# Training error : 0.003889      

# Lets use linear model to predit.
SVM_Linear_Predection<- predict(SVM_Linear, test_data)
SVM_Linear_Conf<- confusionMatrix(SVM_Linear_Predection, test_data$Numbers)
SVM_Linear_Conf
#Overall Model Accuracy= 88.32%
#95% CI: (0.8767, 0.8894)

# Lets try Polynomial SVM Model
SVM_Poly <- ksvm(Numbers~.,data=Sample_Training_data, scale=FALSE,kernel="polydot")
SVM_Poly
SVM_Poly_Predection<- predict(SVM_Poly, test_data)
SVM_Poly_Conf<- confusionMatrix(SVM_Poly, test_data$Numbers)
SVM_Linear_Conf

#Accuracy : 0.8832      88.32%     
#95% CI : (0.8767, 0.8894)

#Hence we will not consider SVM_Liear and SVM_Polynomial models further.
      
# We will build RBF model with non-linear.
SVM_RBF_Model <- ksvm(Numbers~.,
                            data=Sample_Training_data,
                            scale=FALSE,
                            kernel="rbfdot")
SVM_RBF_Model
#Cost [C=1]
#Sigma is 0.00221532160751121 (2.215 e-3)
#Vector 3565
#Training Errr is 0.026556
      
SVM_RBF_Predicted<- predict(SVM_RBF_Model, test_data)
RBFmod_conf_mat<- confusionMatrix(SVM_RBF_Predicted, test_data$Numbers)
RBFmod_conf_mat
#Overall Accuracy is 95.67%
#95% CI : (0.9525, 0.9606)
# Accuracy Improved by 7.35% comparing previous model
# Sensitivity and Specificity has imrpoved and proper
  
      
#Let us use Hyperparameter tuning for RBF model using a three fold Cross validation process.
##  NOTE : We can use Five fold too. COnsidering the time of execution , I have restricted to 3 fold.
# Sigma was 2.215e-3 
#Hence we will use sigma range 2.215e-3, 3.215e-3, 4.215e-3 in fold Cross validation process.
# We will use cost values as 1,2,3
#We will create a  Cross Validation Control Table for easy execution
CVC <- trainControl(method = "cv", number = 3, verboseIter=TRUE)
eval_parameter <- "Accuracy"
set.seed(90)
      
# THE BELOW  PROCESS WILL TAKE 25 to 30 minutes
HPG <- expand.grid(.sigma = c(2.215e-3, 3.215e-3, 4.215e-3),.C=c( 1, 2, 3))
RBF_nonlin_tuned_mod.fit<- train(Numbers~.,
            data=Sample_Training_data,
            method="svmRadial",
            metric=eval_parameter,
            tuneGrid=HPG,
           trControl=CVC)
      RBF_nonlin_tuned_mod.fit
      
#      Following are the resultsResampling results across tuning parameters:
        
#        sigma     C  Accuracy   Kappa    
#      0.002215  1  0.9457778  0.9397322
#      0.002215  2  0.9507771  0.9452889
#      0.002215  3  0.9534440  0.9482532
#      0.003215  1  0.9522219  0.9468956
#      0.003215  2  0.9568885  0.9520819
#      0.003215  3  0.9578885  0.9531935
#      0.004215  1  0.9561107  0.9512181
#      0.004215  2  0.9585553  0.9539342
#      0.004215  3  0.9595553  0.9550462
      
#        Fitting sigma = 0.00421, C = 3 on full training set
#      Accuracy was used to select the optimal model using the largest value.
#      The final values used for the model were sigma = 0.004215 and C = 3.    
      

#Plotting the Results of model Accuracy for each combination of sigma and cost
plot(RBF_nonlin_tuned_mod.fit)

#Building the Final RBF model with tuned hyperparameters of Sigma and Cost values
      RBF_Final <- ksvm(Numbers~.,
                        data=Sample_Training_data,
                        kernel="rbfdot",
                        scale=FALSE,
                        C=3,
                        kpar=list(sigma=4.215e-3))
      
RBF_Final
# Sigma is 0.00422
# C=3
# Suppor Vectors = 3894
# Training error is 0.000444
# The results of above is good. Hence we will predit with test data.

#Model Evaluation of Final RBF Model on Training data
RBF_Final_predict_label <- predict(RBF_Final, Sample_Training_data)
RBF_final_predit_confmatrix<- confusionMatrix(RBF_Final_predict_label, Sample_Training_data$Numbers)
RBF_final_predit_confmatrix

#Overall Accuracy is 99.96%
#95% CI : (0.9989, 0.9999)
#Kappa : 0.9995 
#Sensitivity and Specificity are good
#Results are satisfactory
      
#Final Model's Performance on Test Data by prediction
RBF_Final_test_prediction <- predict(RBF_Final, test_data)
RBF_final_test_ConfusionMtx<- confusionMatrix(RBF_Final_test_prediction, test_data$Numbers)
RBF_final_test_ConfusionMtx

#Accuracy is 96.71%
#Kappa : 0.9634   
#95% CI : (0.9634, 0.9705)
#Sensitivity and Specificity are good.

# Final Conclusion #
# Since there is a less difference between train and test accuracy. We can assume the final model is able to 
#predict correct digits using Non-Linear SVM.
# Maximum accuracy is achived using C=3 and Sigma 4.215e-3, the train accuracy is also very good.
# Further we can finetune using different C and Sigma values.
########### END ############

  