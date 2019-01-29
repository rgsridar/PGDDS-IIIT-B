#Business Understanding-hr_data company employes 4000 people and is experience annual attrition of 15%
#Goal: model the probability of attrition using a logistic regression.what factors they should focus on, in order to curb attrition.
###################################################################

#Data Understanding
#install and load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret")
#install.packages("cowplot")
#install.packages("ggplot2")


library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

#--------------------------------------------------------------------
# Reading datasets
#--------------------------------------------------------------------
empsurvey_data<-read.csv("employee_survey_data.csv", stringsAsFactors = F)
mgrsurvey_data<-read.csv("manager_survey_data.csv", stringsAsFactors = F)
general_data<-read.csv("general_data.csv",stringsAsFactors = F)
in_time<-read.csv("in_time.csv", stringsAsFactors = F)
out_time<-read.csv("out_time.csv", stringsAsFactors = F)

str(empsurvey_data) #4410 obs. of  4 variables
str(mgrsurvey_data) #4410 obs. of  3 variables
str(general_data) #4410 obs. of  24 variables
str(in_time) #4410 obs. of  262 variables
str(out_time) #4410 obs. of  262 variables

#collate the data together in one file
length(unique(tolower(empsurvey_data$EmployeeID))) #4410, confirming EmployeeID is key
length(unique(tolower(mgrsurvey_data$EmployeeID))) #4410, confirming EmployeeID is key
length(unique(tolower(general_data$EmployeeID))) #4410, confirming EmployeeID is key
length(unique(tolower(in_time$X))) #4410, confirming that the 1st column is key
length(unique(tolower(out_time$X))) #4410, confirming that the 1st column is key

setdiff(empsurvey_data$EmployeeID, mgrsurvey_data$EmployeeID) #identical EmployeeID across these datasets
setdiff(empsurvey_data$EmployeeID, general_data$EmployeeID) #identical EmployeeID across these datasets
setdiff(empsurvey_data$EmployeeID, in_time$X) #identical column across these datasets

setdiff(empsurvey_data$EmployeeID, out_time$X)#identical column across these datasets

#merge the general, employee survey and manager survey files
hr_data<-merge(empsurvey_data,mgrsurvey_data,by="EmployeeID", all = F)
hr_data<-merge(hr_data, general_data, by="EmployeeID", all=F)

summary(hr_data) #4410 obs. of 29 variables
#Holidays -- Dates on which no attendance has been recorded/all the employees have NA values
in_time<-in_time[,-which(sapply(in_time, function(x)all(is.na(x)))==TRUE)]  #-- Removed Holidays
out_time<-out_time[,-which(sapply(out_time, function(x)all(is.na(x)))==TRUE)] #-- Removed Holidays

#use the in_time and out_time datasets to compute the average working time for employees

in_time<-cbind(stack(in_time[,-1]), in_time[,1])
colnames(in_time)<-c("In_time", "Date", "EmployeeID")

out_time<-cbind(stack(out_time[,-1]), out_time[,1])
colnames(out_time)<-c("Out_time", "Date", "EmployeeID")

setdiff(in_time$EmployeeID, out_time$EmployeeID)#identical EmployeeID in both datasets
setdiff(in_time$Date, out_time$Date)#identical EmployeeID in both datasets

#merge in and out datasets 
in_out<-merge(in_time, out_time, by= c("EmployeeID","Date"), all=F) #1098090 obs. of 4 variables

in_out$In_time<-as.POSIXct(gsub("-", "/", in_out$In_time), format = "%Y/%m/%d %H:%M:%S")
in_out$Out_time<-as.POSIXct(gsub("-","/", in_out$Out_time), format="%Y/%m/%d %H:%M:%S")

in_out$workhours<-in_out$Out_time-in_out$In_time #computes work hour per employee
avg_workhours<-aggregate(as.numeric(workhours)~EmployeeID, in_out, mean)
colnames(avg_workhours)<-c("EmployeeID", "Average Working Hours")

hr_data<-merge(hr_data,avg_workhours,by="EmployeeID", all=F)#master file 4410 obs. of 30 variables




##################################################################################
#Data Preparation and Exploratory Data Analysis
##################################################################################
#check the structure of the master file
str(hr_data)
#------------------------------------------------
#--Treating NA Values
#------------------------------------------------
sum(is.na(hr_data)) #111 NA records
sapply(hr_data, function(x) sum(is.na(x))) #shows there are NAs in 5 columns
#5 columns are
#EnvironmentSatisfaction-25 #JobSatisfaction-20 #WorkLifeBalance-38 #NumCompaniesWorked-19
#TotalWorkingYears-9
#Total NA values are : 111/4410 = 2.5% of the entire data. We can afford to remove them.
hr_data<-na.omit(hr_data)
#hr_data has 4300 obs of 30 variables


sum(is.na(hr_data)) #all NA values are removed

#----------------------------------------------
#Outlier treatment
#----------------------------------------------
### Include all outliers in one plot 



str(hr_data) #shows numeric values to apply outlier treatment

par(mfrow=c(2,4))

boxplot(hr_data$Age,main="Age")
quantile(hr_data$Age, seq(0,1,0.1)) #no outliers

boxplot(hr_data$DistanceFromHome,main="DistanceFromHome")
quantile(hr_data$DistanceFromHome, seq(0,1,0.1)) #no outliers

boxplot(hr_data$Education,main="Education")
quantile(hr_data$Education, seq(0,1,0.1)) #no outliers

boxplot(hr_data$JobLevel,main="JobLevel")
quantile(hr_data$JobLevel, seq(0,1,0.1)) #no outliers

boxplot(hr_data$MonthlyIncome,main="MonthlyIncome")
quantile(hr_data$MonthlyIncome, seq(0,1,0.1)) #Sudden spike after 80%
hr_data$MonthlyIncome[which(hr_data$MonthlyIncome>68850)]<-68850 # treated outliers


boxplot(hr_data$NumCompaniesWorked,main="NumCompaniesWorked")
quantile(hr_data$NumCompaniesWorked, seq(0,1,0.1)) #no outliers

boxplot(hr_data$PercentSalaryHike,main="PercentSalaryHike")
quantile(hr_data$PercentSalaryHike, seq(0,1,0.1)) #no outliers

boxplot(hr_data$StandardHours,main="StandardHours")
quantile(hr_data$StandardHours, seq(0,1,0.01)) #no outliers

boxplot(hr_data$StockOptionLevel,main="StockOptionLevel")
quantile(hr_data$StockOptionLevel, seq(0,1,0.1)) #no outliers

boxplot(hr_data$TotalWorkingYears,main="TotalWorkingYears")
quantile(hr_data$TotalWorkingYears, seq(0,1,0.1))#Sudden spike after 90%
hr_data$TotalWorkingYears[which(hr_data$TotalWorkingYears>17)]<-17 #treated outliers


boxplot(hr_data$TrainingTimesLastYear,main="TrainingTimesLastYear")
quantile(hr_data$TrainingTimesLastYear, seq(0,1,0.1)) #no outliers

boxplot(hr_data$YearsAtCompany,main="YearsAtCompany")
quantile(hr_data$YearsAtCompany, seq(0,1,0.1)) #Sudden spike after 90%
hr_data$YearsAtCompany[which(hr_data$YearsAtCompany>10)]<-10 #remove outliers


boxplot(hr_data$YearsSinceLastPromotion,main="YearsSinceLastPromotion")
quantile(hr_data$YearsSinceLastPromotion, seq(0,1,0.1)) #Sudden spike after 100%
hr_data$YearsSinceLastPromotion[which(hr_data$YearsSinceLastPromotion>7)]<-7 #remove outliers


boxplot(hr_data$YearsWithCurrManager,main="YearsWithCurrManager")
quantile(hr_data$YearsWithCurrManager, seq(0,1,0.1)) #Sudden spike after 90%
hr_data$YearsWithCurrManager[which(hr_data$YearsWithCurrManager>9)]<-9


boxplot(hr_data$`Average Working Hours`,main="Average Working Hours")
quantile(hr_data$`Average Working Hours`, seq(0,1,0.1)) #no outliers

#now all outliers are treated
#Lets rename the numeric levels of these variables as per data dictionary to understand them better 
hr_data$Education <- ifelse(hr_data$Education==1, "Below College",
                                      ifelse(hr_data$Education==2, "College", 
                                             ifelse(hr_data$Education==3, "Bachelor",
                                                    ifelse(hr_data$Education==4, "Master", "Doctor"))))
hr_data$EnvironmentSatisfaction <- ifelse(hr_data$EnvironmentSatisfaction==1, "Low",
                                                    ifelse(hr_data$EnvironmentSatisfaction==2, "Medium", 
                                                           ifelse(hr_data$EnvironmentSatisfaction==3, "High", "Very High")))
hr_data$JobInvolvement <- ifelse(hr_data$JobInvolvement==1, "Low",
                                           ifelse(hr_data$JobInvolvement==2, "Medium", 
                                                  ifelse(hr_data$JobInvolvement==3, "High", "Very High")))
hr_data$JobSatisfaction <- ifelse(hr_data$JobSatisfaction==1, "Low",
                                            ifelse(hr_data$JobSatisfaction==2, "Medium", 
                                                   ifelse(hr_data$JobSatisfaction==3, "High", "Very High")))
hr_data$PerformanceRating <- ifelse(hr_data$PerformanceRating==1, "Low",
                                              ifelse(hr_data$PerformanceRating==2, "Good", 
                                                     ifelse(hr_data$PerformanceRating==3, "Excellent", "Outstanding")))
hr_data$WorkLifeBalance <- ifelse(hr_data$WorkLifeBalance==1, "Bad",
                                            ifelse(hr_data$WorkLifeBalance==2, "Good", 
                                                   ifelse(hr_data$WorkLifeBalance==3, "Better", "Best")))


#########################################################################
#next look at uni-variate analysis to determine the correlation between attrition and other variables

str(hr_data) #comprises 22 varaibles that are num/int and 8 character variables
library(gridExtra)
#uni-variate analysis
#check correlation between attrition and age and education
g1<-ggplot(hr_data, aes(Education, Gender, color = as.factor(Attrition)))+ geom_point(alpha=0.2)+geom_jitter()
#higher attrition in those with education less than level 4(Masters)


#check correlation between attrition and monthly income
g2<-ggplot(hr_data, aes(MonthlyIncome, Gender, color=as.factor(Attrition)))+geom_point()
#low and high monthly income in men and women quitting so perhaps this is not the main reason

#check the correlation between job satisfaction and attrition
g3<-ggplot(hr_data, aes(JobSatisfaction, EnvironmentSatisfaction, color=as.factor(Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#shows that lower the job satisfaction and environment satisfaction, the higher is the attrition

#check the correlation between worklifebalance and attrition
g4<-ggplot(hr_data, aes(WorkLifeBalance, Gender, color=as.factor(hr_data$Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#lower the work life balance, higher the attrition

#check the correlation between Total working years and Attrition
g5<-ggplot(hr_data, aes(TotalWorkingYears,JobRole, color=as.factor(Attrition)))+geom_point()
#it is clear that there is less attrition in employees who have worked for more than 10 years

#check the effect of average working hours on attrition
g6<-ggplot(hr_data, aes(hr_data$`Average Working Hours`,hr_data$Gender, color=as.factor(hr_data$Attrition)))+geom_point()+geom_jitter()
#higher the average working hours, higher the attrition

#coorelation of age on attrition
g7<-ggplot(hr_data, aes(Age, Gender, color=as.factor(Attrition)))+geom_point()
#higher the age, lower the attrition. Low after the age of 40

#check the effect of attrition on departments
g8<-ggplot(hr_data, aes(hr_data$Department, Gender, color=as.factor(Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#distribution of attrition is similar indicates its across the company

#marital status and attrition 
g9<-ggplot(hr_data, aes(MaritalStatus, Gender, color=as.factor(Attrition)))+geom_point()+geom_jitter()
#attrition is higher among those single compared to married and divorced employees

#check the correlation between attrition and number of companies worked previously
g10<-ggplot(hr_data, aes(hr_data$NumCompaniesWorked, Gender,color= as.factor(Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#higher the number of companies worked previously, higher the attrition

#check the effect of promotion timing on attrition
g11<-ggplot(hr_data, aes(hr_data$YearsSinceLastPromotion, Gender, color=as.factor(Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#the higher the gap in promotion duration, the higher the attrition

#time spent with current manager on attrition
g12<-ggplot(hr_data, aes(hr_data$YearsWithCurrManager, Gender, color=as.factor(Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#higher attrition in those who spend less than 2.5 years with a given manager

#correlation between Business travel and attrition
g13<-ggplot(hr_data, aes(BusinessTravel, Gender, color=as.factor(Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#those who travel frequently show high attrition

#marital status and business travel versus attrition
g14<-ggplot(hr_data, aes(BusinessTravel, MaritalStatus, color=as.factor(Attrition)))+geom_point(alpha=0.2)+geom_jitter()
#those who are single and travel frequently are highly likely to leave

#factors that contribute to attrition are: marital status-single, business travel-frequent travel,
#<2.5 years spent with current manager, higher gap in promotion in years, worked in more number of companies previosuly,
#average working hours-higher, higher the age, education less than masters
grid.arrange(g1,g2,g3,g4)
grid.arrange(g5,g6,g7,g8)
grid.arrange(g9,g10,g11)
grid.arrange(g12,g13,g14)
#############################################################################################

#Feature standardisation
#############################################################################################
#Normalising continuous variables
#############################################################################################
str(hr_data)

hr_data$Age<-scale(hr_data$Age)
hr_data$DistanceFromHome<-scale(hr_data$DistanceFromHome)
hr_data$MonthlyIncome <-scale(hr_data$MonthlyIncome)
hr_data$NumCompaniesWorked<-scale(hr_data$NumCompaniesWorked)
hr_data$PercentSalaryHike<-scale(hr_data$PercentSalaryHike)
hr_data$TotalWorkingYears<-scale(hr_data$TotalWorkingYears)
hr_data$TrainingTimesLastYear<-scale(hr_data$TrainingTimesLastYear)
hr_data$YearsAtCompany<-scale(hr_data$YearsAtCompany)
hr_data$YearsSinceLastPromotion<-scale(hr_data$YearsSinceLastPromotion)
hr_data$YearsWithCurrManager<-scale(hr_data$YearsWithCurrManager)
hr_data$`Average Working Hours`<-scale(hr_data$`Average Working Hours`)

#check attrition rate= sum(Attrition) /total number of rows in the master dataframe hr_data
#change the variableto binary ie numeric 0 and 1 where yes=1 and no=0

hr_data$Attrition<-ifelse(hr_data$Attrition=="Yes", 1,0)

Attrition_rate<-sum(hr_data$Attrition)/nrow(hr_data)
Attrition_rate #rate of attritin is 16.2%
# Remove columns having single value
index<- sapply(sapply(hr_data,FUN=unique),FUN=length)
sort(index)
#EmployeeCount                  Over18           StandardHours
#These are the 3 fields having only one entry which is constant through out. So we can remove them
hr_data<-hr_data[,-which(index==1)]


#-------------------------------------------------------------------------------------------------------------------
#DUMMY VARIABLE CREATION.
#-------------------------------------------------------------------------------------------------------------------

#Let us first look at all the factor type variables. We can not use them as it is in a logistic regression model.
#We should create dummy variables.
#For each column, if there are n levels of characters, we will create n-1 dummies.
#-------------------------------------------------------------------------------------------------------------------

#convert categorical attributes to factor
str(hr_data) 
# creating a dataframe of categorical features, StockOptionLevel & Job level, although a numeric field is actually a categorical variable
index<-which(sapply(X = hr_data,FUN=class)=="character")
hr_data_chr<- hr_data[,c(index,15,21)]

hr_data_factor<-data.frame(sapply(hr_data_chr, function(x) factor(x)))
str(hr_data_factor) #categorical are converted into numbers

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_data_factor, function(x) data.frame(model.matrix(~x-1,data =hr_data_factor)[,-1])))
# For variables having only two levels, such as Gender "Male" is 1,
#Attrition "Yes" is 1 and PerformanceRating "Outstading" is 1 
#Correlation Check
hr_corelation_matrix <- cor(hr_data[,-c(1,index,15,21)])
library(corrplot)
par(mfrow=c(1,1))
corrplot(hr_corelation_matrix, method = "number", title = "Correlation Matrix", type = "lower", order = "FPC", number.cex = 0.8, tl.cex = 0.7,col = colorRampPalette(c("darkred","grey","red"))(100)) 



#create the master file by joining the dummies dataset
hr_data_final<-cbind(hr_data[,-c(index,15,21)],dummies)
str(hr_data_final)#4300 obs. of 57 variables 




###########################################################################
#splitting the data between test and train

set.seed(100)
indices= sample.split(hr_data_final$Attrition, SplitRatio = 0.7)
train=hr_data_final[indices,]
test=hr_data_final[!(indices),]

###############################################################################
#logistic regression

model_1 = glm(Attrition ~ ., data = train[,-1], family = "binomial")#remove Employee ID dependant variable
summary(model_1)
#AIC: 2107.4  Null deviance: 2661.4  on 3009  degrees of freedom
#Residual deviance: 1995.4  on 2954  degrees of freedom

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2) #  Null deviance: 2661.4  on 3009  degrees of freedom
#Residual deviance: 2008.4  on 2973  degrees of freedom
#AIC: 2082.4

# Removing multicollinearity through VIF check
library(car)
sort(vif(model_2))
#EducationField.xLife.Sciences,EducationField.xMedical have really high vif value but they are highly
#significant as well. So we can't remove them at this stage. YearsAtCompany has vif = 4.89 & the p value is high as well:0.068691
#Removing YearsAtCompany
model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + EducationField.xLife.Sciences + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
               MaritalStatus.xSingle + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
               StockOptionLevel.x3, family = "binomial", data = train[,-1])

summary(model_3) #AIC :2083.8
sort(vif(model_3))

#next remove EducationField.xLife.Sciences as its VIF is high(8.740553). Although it's highly significant, 
#it's vif didn't change even after removing one variable from the model.It is still highly correlated with some other variable
model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
               MaritalStatus.xSingle + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
               StockOptionLevel.x3, family = "binomial", data = train[,-1])
summary(model_4) #AIC: 2098.6
sort(vif(model_4))

#MaritalStatus.xMarried has high VIF(2.157305) and high p-value(0.220575) hence remove in next model
model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
               StockOptionLevel.x3, family = "binomial", data = train[,-1])

summary(model_5) #AIC:2098.1
sort(vif(model_5))

#EducationField.xMarketing to be removed as it has high p-value(0.29497) 
model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
               StockOptionLevel.x3, family = "binomial", data = train[,-1])
summary(model_6)#AIC:2097.2
sort(vif(model_6)) 

# Removed EducationField.xMedical as it has high p value = 0.415721
model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
               StockOptionLevel.x3, family = "binomial", data = train[,-1])
summary(model_7)#AIC: 2095.9
# Removed EducationField.xOther as it has high p value = 0.196931
model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
               StockOptionLevel.x3, family = "binomial", data = train[,-1])
summary(model_8)#AIC: 2095.6
# Removed EducationField.xTechnical.Degree as it has high p value = 0.198851
model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  
               MaritalStatus.xSingle + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
               StockOptionLevel.x3, family = "binomial", data = train[,-1])
summary(model_9)#AIC: 2095.4
sort(vif(model_9))
#TOtalWorkingYears has higher VIF but low p-value so retain
# Removed JobLevel.x5 as it has high p value = 0.154969
model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
               JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Education.xDoctor + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
              JobLevel.x2 + StockOptionLevel.x1 + StockOptionLevel.x3, family = "binomial", data = train[,-1])
summary(model_10)#AIC: 2095.5
# Removed StockOptionLevel.x3 as it has high p value = 0.127918
model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + JobInvolvement.xMedium + JobInvolvement.xVery.High + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Education.xDoctor + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 + StockOptionLevel.x1 ,family = "binomial", data = train[,-1])
summary(model_11)#AIC: 2096
# Removed JobInvolvement.xVery.High as it has high p value = 0.112398
model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + JobInvolvement.xMedium +  
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Education.xDoctor + JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 + StockOptionLevel.x1 ,family = "binomial", data = train[,-1])
summary(model_12)#AIC: 2096.4
#Removing Education.xDoctor as it has high p value = 0.111133
model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + JobInvolvement.xMedium +  
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 + StockOptionLevel.x1 ,family = "binomial", data = train[,-1])
summary(model_13)#AIC: 2097.2
#Removing JobRole.xHuman.Resources as it has high p value = 0.084547
model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + JobInvolvement.xMedium +  
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 + StockOptionLevel.x1 ,family = "binomial", data = train[,-1])
summary(model_14)#AIC: 2098.5
#Removing JobInvolvement.xMedium as it has high p value = 0.083084
model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 + StockOptionLevel.x1 ,family = "binomial", data = train[,-1])
summary(model_15)#AIC: 2098.5
#Removing StockOptionLevel.x1 as it has high p value = 0.05222
model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 ,family = "binomial", data = train[,-1])
summary(model_16)#AIC: 2101.2
#Removing JobRole.xManager as it has high p value = 0.033857
model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 ,family = "binomial", data = train[,-1])
summary(model_17)#AIC: 2104.1
#Removing EnvironmentSatisfaction.xVery.High as it has high p value = 0.029232
model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                JobLevel.x2 ,family = "binomial", data = train[,-1])
summary(model_18)#AIC: 2106.9
#Removing JobLevel.x2 as it has high p value = 0.016872
model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle 
               ,family = "binomial", data = train[,-1])
summary(model_19)#AIC: 2110.6
#Removing JobRole.xResearch.Director as it has high p value = 0.003563
model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + JobRole.xSales.Executive +  MaritalStatus.xSingle 
              ,family = "binomial", data = train[,-1])
summary(model_20)#AIC: 2116.5
#Removing JobRole.xSales.Executive as it has high p value = 0.011148
model_21<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle 
              ,family = "binomial", data = train[,-1])
summary(model_21)#AIC: 2120.9
#Removing BusinessTravel.xTravel_Rarely as it has high p value = 0.001926
model_22<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Working Hours` + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle 
              ,family = "binomial", data = train[,-1])
summary(model_22)#AIC: 2129.6
#Let's check the vif once again
sort(vif(model_22))


#All the variables have low p-value total of 16 variables can be treated as final model

final_model<-model_22
summary(final_model) #AIC: 2129.6

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
summary(test_pred)

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test$prob <- test_pred
test

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


conf_matrix<-table(test_pred_attrition,test_actual_attrition)
library(caret)
specificity(conf_matrix) #0.3205742
sensitivity(conf_matrix)#0.9648474
#Accuracy=0.86

##########################################################################
#Let's find out the optimal probalility cutoff 
optimal_cutoff<-function(cutoff)
{
test_pred_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive="Yes")
acc<-test_conf$overall[1]
sens<-test_conf$byClass[1]
spec<-test_conf$byClass[2]
out <- t(as.matrix(c(sens, spec, acc)))
colnames(out)<-c("sensitivity","specificity","accuracy")
return(out)
}
###############################################################################
#next, create cutoff values from 0.01 to 0.8 and initialise a matrix of 100*3
#This results in high accuracy and high specificity but low value of sensitivity of.36.
#Hence reducing the range of cutoff values to help balance the specificity and sensitivity.

# Summary of test probability

summary(test_pred)

s = seq(0.01,0.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = optimal_cutoff(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.5,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"),cex = 0.75)


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


cutoff <- mean(cutoff) #0.1775758 

#optimal cutoff value which gives the best sensitivity and specificity is 0.1775758
#use cutoff value of 0.1775758 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1775758, "Yes", "No"))
library(caret)

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1] #0.7596899

sens <- conf_final$byClass[1] # 0.7607656 

spec <- conf_final$byClass[2]#0.759482 

acc

sens

spec

View(test)

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

AUC<-performance(pred_object_test,measure='auc')
AUC<-round(as.numeric(AUC@y.values[[1]]),1)
AUC
#Area under the curve: 0.8 which is expected from a good model.
par(mfrow=c(1,1))
plot(performance_measures_test, main = "ROC Curve :  Area under the curve 0.8")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.5202475
# KS- statistic is a measure of how well our model is able to distinguish between the Good and the Bad categories.
#The acceptable value should be above 40%. For our model it is 52%.

##############################################################################

# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = data.frame(lift(test_actual_attrition, test_pred, groups = 10))
Attrition_decile
#For a good model, it is expected that 80% of the Bad is captured within top 4 deciles.
#Our model is able to capture 81.8% of the bad, within top 4 deciles.
