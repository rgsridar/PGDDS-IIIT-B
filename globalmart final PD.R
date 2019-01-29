

# Installing packages

# Install the required packages
requiredPackages = c('forecast','tseries','graphics','dplyr','ggplot2','lubridate',
                     'stats')

#Installing the required packages
for(item in requiredPackages){
  if(!require(item,character.only = TRUE)){
    #install.packages(item)
  }
  library(item, character.only = TRUE)
}


# Our Objective 
# We need to identify most consistently profitable markets - Top two
# We need to build a time series model to forecast demand and sales for the top tow markets using classical decomposition and auto arima
# We will perform model evaluation using MAPE and accuracy and predict the Demand and Sales of the next 6 months for the 2 Market Buckets


# Data understanding

#Import data to main_db
main_db<- read.csv("Global Superstore.csv", stringsAsFactors = FALSE, na.strings = c(" ", "", "NA", "na", "N/A", "n/a"))

#Understanding the data
dim(main_db)
#There are 51,290 records with 24 attributes.
str(main_db)
# Order.Date and Shipping.Date are in charecter format we will convert to data.As we can see Order.Date are Shipping.Date are represented as character format. We will convert them to Date Objects.
# we can also eliminate Row.ID as there is no significance.

main_db<- main_db[,-1]
main_db$Order.Date<-as.Date(main_db$Order.Date,"%d-%m-%Y")
main_db$Ship.Date<-as.Date(main_db$Ship.Date,"%d-%m-%Y")

#Check for duplicate
length(which(duplicated(main_db)==TRUE))  # No duplicate rows or identical records.

#Checking if Order.ID is the primary key of this dataset
length(unique(main_db$Order.ID))
length(unique(main_db$Customer.ID))
length(unique(main_db$Product.ID))
# Yes OrderID can be primary key.

#Checking the dataset for Missing Values.
sapply(main_db, function(x) sum(is.na(x))) # Postal ocdes has 41296 missing records. Only US clients Postal code is recorded.
#---- Check
P.code_remove<-main_db[which(main_db$Country!="United States"),]
sum(is.na(P.code_remove$Postal.Code))
rm(P.code_remove)
# We did this check to confirm that all non-US clients Postal code is missing.
#However we won't be removing anything since postal codes are not our concern here
#We will work with Market level data.
str(main_db)
main_db$Postal.Code<-as.factor(main_db$Postal.Code) 

# Order.data ranges from 1st January 2011 to 31st December 2014 - 48 months.
# We 
#It is clear that the Order.Date information recorded ranges from 1st Jan 2011 to 31st Dec 2014. 
#We have 48 months of recorded information.
#We have derived a New Metric named Month.number We will assign month numbers like January 2011 = 1 .... December 2014 will be 48 . We will use
# this as a time stamp for our model.

main_db$Month.number <- sapply(main_db$Order.Date, function(x) length(seq(from= min(main_db$Order.Date), to=x, by='month')))
range(main_db$Month.number) # We have a range of 1 to 48 


# Data understanding

#Let us understand the market and segment

unique(main_db$Segment) # "Consumer"    "Corporate"   "Home Office"

unique(main_db$Market) # "US"     "APAC"   "EU"     "Africa" "EMEA"   "LATAM"  "Canada"

unique(main_db$Category) # "Technology"      "Furniture"       "Office Supplies"

# we will create following buckets
# Africa Consumer = AF1
# Africa Corporate= AF2
# Africa Home Office = AF3
# Similarly for US1,US2,US3 etc....


#Africa 
AF1<- subset(main_db, main_db$Segment=="Consumer" & main_db$Market=="Africa")
AF2<- subset(main_db, main_db$Segment=="Corporate" & main_db$Market=="Africa")
AF3<- subset(main_db, main_db$Segment=="Home Office" & main_db$Market=="Africa")

#APAC 
APAC1<- subset(main_db, main_db$Segment=="Consumer" & main_db$Market=="APAC")
APAC2<- subset(main_db, main_db$Segment=="Corporate" & main_db$Market=="APAC")
APAC3<- subset(main_db, main_db$Segment=="Home Office" & main_db$Market=="APAC")

#Canada 
CAN1<- subset(main_db, main_db$Segment=="Consumer" & main_db$Market=="Canada")
CAN2<- subset(main_db, main_db$Segment=="Corporate" & main_db$Market=="Canada")
CAN3<- subset(main_db, main_db$Segment=="Home Office" & main_db$Market=="Canada")

#EMEA 
EMEA1<- subset(main_db, main_db$Segment=="Consumer" & main_db$Market=="EMEA")
EMEA2<- subset(main_db, main_db$Segment=="Corporate" & main_db$Market=="EMEA")
EMEA3<- subset(main_db, main_db$Segment=="Home Office" & main_db$Market=="EMEA")

#EU 
EU1<- subset(main_db, main_db$Segment=="Consumer" & main_db$Market=="EU")
EU2<- subset(main_db, main_db$Segment=="Corporate" & main_db$Market=="EU")
EU3<- subset(main_db, main_db$Segment=="Home Office" & main_db$Market=="EU")

#LATAM
LATM1<- subset(main_db, main_db$Segment=="Consumer" & main_db$Market=="LATAM")
LATM2<- subset(main_db, main_db$Segment=="Corporate" & main_db$Market=="LATAM")
LATM3<- subset(main_db, main_db$Segment=="Home Office" & main_db$Market=="LATAM")

#USA
US1<- subset(main_db, main_db$Segment=="Consumer" & main_db$Market=="US")
US2<- subset(main_db, main_db$Segment=="Corporate" & main_db$Market=="US")
US3<- subset(main_db, main_db$Segment=="Home Office" & main_db$Market=="US")


# We will now group 21 markets based on Month.number (Time Stamp), profits,sales and quantity.
#Coefficient of variation: Std Deviation of Monthly Profit/Mean of Monthly Profit
#Lowest coefficient of variation is better.
Grouping<- function(input_df)
{
  input_df$Month.number<- as.numeric(input_df$Month.number)
  input_df<- input_df %>% group_by(Month.number) %>% summarise(Monthly.Profit = sum(Profit), Monthly.Sales= sum(Sales), Monthly.Demand= sum(Quantity)) %>% 
    mutate(Net.Profit = sum(Monthly.Profit), Average.Profit = mean(Monthly.Profit), Coeff.Var= (sd(Monthly.Profit)/mean(Monthly.Profit)))
  input_df<- sapply(input_df, function(x) round(x,2))
  return(data.frame(input_df))
}

# We will do Grouping for 21 market buckets 
AF1<- Grouping(AF1)
AF2<- Grouping(AF2)
AF3<- Grouping(AF3)
APAC1<- Grouping(APAC1)
APAC2<- Grouping(APAC2)
APAC3<- Grouping(APAC3)
CAN1<- Grouping(CAN1)
CAN2<- Grouping(CAN2)
CAN3<- Grouping(CAN3)
EMEA1<- Grouping(EMEA1)
EMEA2<- Grouping(EMEA2)
EMEA3<- Grouping(EMEA3)
EU1<- Grouping(EU1)
EU2<- Grouping(EU2)
EU3<- Grouping(EU3)
LATM1<- Grouping(LATM1)
LATM2<- Grouping(LATM2)
LATM3<- Grouping(LATM3)
US1<- Grouping(US1)
US2<- Grouping(US2)
US3<- Grouping(US3)

#================================
#Outlier Checks
#================================
boxplot(AF1$Monthly.Sales,main="AF Consumer Sales")
boxplot(AF2$Monthly.Sales,main="AF Corporate Sales")
boxplot(AF3$Monthly.Sales,main="AF Home Office Sales")

boxplot(AF1$Monthly.Demand,main="AF Consumer Demand")
boxplot(AF2$Monthly.Demand,main="AF Corporate Demand")
boxplot(AF3$Monthly.Demand,main="AF Home Office Demand")
# We will now identify 2 of the most Profitable markets based on their coefficient of variation.

Most_profit_market<-data.frame(Net.Profit=c(AF1[1, 5],AF2[1, 5],AF3[1, 5],APAC1[1, 5],APAC2[1, 5],
                        APAC3[1, 5],CAN1[1, 5],CAN2[1, 5],CAN3[1, 5],EMEA1[1, 5],
                        EMEA2[1, 5],EMEA3[1, 5],EU1[1, 5],EU2[1, 5],EU3[1, 5],
                        LATM1[1, 5],LATM2[1, 5],LATM3[1, 5],US1[1, 5],US2[1, 5],US3[1, 5]), 
                        
                        Average.Profit=c(AF1[1, 6],AF2[1, 6],AF3[1, 6],APAC1[1, 6],APAC2[1, 6],APAC3[1, 6],
                       CAN1[1, 6],CAN2[1, 6],CAN3[1, 6],EMEA1[1, 6],EMEA2[1, 6],EMEA3[1, 6],
                       EU1[1, 6],EU2[1, 6],EU3[1, 6],LATM1[1, 6],LATM2[1, 6],LATM3[1, 6],
                       US1[1, 6],US2[1, 6],US3[1, 6]), 
                       
                       Coeff.Var=c(AF1[1, 7],AF2[1, 7],AF3[1, 7], APAC1[1, 7],APAC2[1, 7],APAC3[1, 7],
                       CAN1[1, 7],CAN2[1, 7],CAN3[1, 7],EMEA1[1, 7],EMEA2[1, 7],EMEA3[1, 7],
                       EU1[1, 7],EU2[1, 7],EU3[1, 7],LATM1[1, 7],LATM2[1, 7],LATM3[1, 7],
                       US1[1, 7],US2[1, 7],US3[1, 7]),
           
                      Mrkt.Bucket = c("AF1","AF2","AF3","APAC1","APAC2","APAC3","CAN1","CAN2",
                      "CAN3","EMEA1","EMEA2","EMEA3","EU1","EU2","EU3","LATM1","LATM2",
                      "LATM3","US1","US2","US3")) %>% arrange(desc(Net.Profit, Average.Profit), Coeff.Var) 
Most_profit_market
#Creating one visualization to observe two combinations having least coeff of variation.
p<-ggplot(data=Most_profit_market, aes(x=factor(Mrkt.Bucket,levels = Most_profit_market$Mrkt.Bucket[order(Most_profit_market$Coeff.Var)]), y=Coeff.Var,fill=factor(Coeff.Var))) +
  geom_bar(stat="identity")+scale_fill_manual(values=c(rep("red",2),rep( "grey50",19)))+
  xlab("Market-Category Combinations") +
  ylab("Coefficient of Variation") +
  ggtitle("Coefficient of Variation of 21 Buckets") 
p
# We can find 
#Net.Profit Average.Profit Coeff.Var Mrkt.Bucket
#1   222817.56        4642.03      0.63       APAC1
#2   188687.71        3930.99      0.62         EU1

# Lets remove unwanted Markets
rm(AF1, AF2, AF3, APAC2, APAC3, CAN1, CAN2, CAN3, EMEA1, 
   EMEA2, EMEA3, EU2, EU3, LATM1, LATM2, LATM3, US1, US2, US3)

# It is evident that Demand and Sales Forecasting model is required for Consumer segment of APAC and EU

# We will now build Time Series Model

# Data for APAC1
# Lets create a dataframe for sales and demand. 
demand.APAC1<- APAC1[,c("Month.number","Monthly.Demand")]
sales.APAC1<- APAC1[,c("Month.number","Monthly.Sales")]
row_count<- nrow(APAC1)

# ASSUMPTION :   WE WILL USE 15% for testing. Hence  42 rows for model building and 6 rows for testing

######  #APAC_CONSUMER Building Model to forecast the demand of the quantity ############

#We will build two demand forecasting models using the below techniques:
#We will use Classical Decomposition and Auto ARIMA
test.demand.APAC1<- demand.APAC1[(row_count-5):row_count,] #Test dataset
demand.APAC1<- demand.APAC1[1:(row_count-6),]  # Training dataset

#Let us begin to understand the individual components of our Demand Time Series
# We will set frequency to 12 to view complete one year

TS_decom.demand.APAC1<- ts(demand.APAC1[,"Monthly.Demand"])
plot(TS_decom.demand.APAC1)# We can see linear in trend with slope

#smoothening the time series
w <-1
Smooth_APAC_demand<-stats::filter(TS_decom.demand.APAC1,

                                  filter=rep(1/(2*w+1), (2*w+1)), method='convolution', sides=2)

length(Smooth_APAC_demand)
Smooth_APAC_demand

#lets now view the smoothened data with the time series data.
plot(TS_decom.demand.APAC1)
lines(Smooth_APAC_demand, col="blue", lwd=2)


#Smoothing left end of the time series
diff <- Smooth_APAC_demand[w+2] - Smooth_APAC_demand[w+1]
for (i in seq(w,1,-1)) {
  Smooth_APAC_demand[i] <- Smooth_APAC_demand[i+1] - diff
}

#Smoothing right end of the time series

n <- length(TS_decom.demand.APAC1)
diff <- Smooth_APAC_demand[n-w] - Smooth_APAC_demand[n-w-1]
for (i in seq(n-w+1, n)) {
  Smooth_APAC_demand[i] <- Smooth_APAC_demand[i-1] + diff}

#Plot the smoothed time series

plot(TS_decom.demand.APAC1)
lines(Smooth_APAC_demand, col="blue", lwd=2)


Smooth_APAC_demand_for_model<-as.data.frame(cbind(demand.APAC1["Month.number"],as.vector(Smooth_APAC_demand)))


colnames(Smooth_APAC_demand_for_model)[2]<-c('Monthly.Demand')

#Building a model on the smoothed time series using classical decomposition


# We will model global trend aspect
# We are choosing a multiplicative model to train the data since it has a seasonal pattern with
# amplitude which is not constant and increases with time.

globaltrend_demand.APAC1 <- lm(Monthly.Demand~ sin(0.5*Month.number)*poly(Month.number,1)+cos(0.1*Month.number)*poly(Month.number,1)+
                                tan(0.02*Month.number), data =Smooth_APAC_demand_for_model )



globcomp_demand.APAC1 <- predict(globaltrend_demand.APAC1,Month.number=demand.APAC1$ Month.number)

summary(globcomp_demand.APAC1)

lines(demand.APAC1$ Month.number, globcomp_demand.APAC1, col="Red",lwd=2)


#Monthly demand shown in Blue and Forecasted Global Component in Red


#We have trend and seasonality.We will Isolate the Local Component and analyze its ARMA components
local_d_APAC1<- demand.APAC1$Monthly.Demand - globcomp_demand.APAC1

#Plotting local component for analyzing
plot(local_d_APAC1,col="Blue",type="l")


acf(local_d_APAC1,lag.max=40) #Less than 5% datapoints are crossing the confidence interval
acf(local_d_APAC1, type="partial",lag.max=40) #Less than 5% datapoints are crossing the confidence interval

#We will now check if this local component can be modeled as AR(p) or MA(q) series.
local_arima_demand_APAC11<- auto.arima(local_d_APAC1)
local_arima_demand_APAC11
#ARIMA(0,0,0) with zero mean ;So AR(p) or MA(q) series in the local component

#classical decomposition model for next 6 months [Month : 43,44,45,46,47,48].
globaltrend_demand.APAC1


Class_decom_demand.APAC1<- predict(globaltrend_demand.APAC1, data.frame(Month.number=test.demand.APAC1$Month.number))
Class_decom_demand.APAC1

#We'll check if the residual series is white noise

resi <- local_d_APAC1-fitted(local_arima_demand_APAC11)

adf.test(resi,alternative = "stationary") #Dickey-Fuller = -4.691, Lag order = 3, p-value = 0.01
kpss.test(resi) #KPSS Level = 0.030788, Truncation lag parameter = 1, p-value = 0.1
plot(resi)
hist(resi,probability = T)
lines(density(x = resi),col="red")
### p-value of adf is <.05 and p-value of kpss is =.1 indicates that residual series is indeed white noise.
#The residuals are also normally distributed.

#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.APAC1 dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE

ACC_CD_demand_APAC1<- accuracy(Class_decom_demand.APAC1, test.demand.APAC1$Monthly.Demand)
ACC_CD_demand_APAC1
mape_clasdec_demand.APAC1<-ACC_CD_demand_APAC1[5]
mape_clasdec_demand.APAC1 #18.79196

#                 ME     RMSE      MAE       MPE     MAPE
#Test set -23.16084 127.4945 107.1754 -8.815864 18.79196


# Demand Time Series im Green and Fitted Classical Decomposition in RED

full_clasdec_demand.APAC1<- ts(c(globcomp_demand.APAC1, Class_decom_demand.APAC1))
plot(ts(APAC1[,"Monthly.Demand"]), col="Green", lwd=2,main="APAC Consumer Demand Forecasting: Classical decomposition")
lines(full_clasdec_demand.APAC1, col="RED", lwd=2)

#We will check Auto Arima Model 
AR_dmd_APAC1<- auto.arima(demand.APAC1$Monthly.Demand)
AR_dmd_APAC1  # ARIMA(0,1,0)


# Demand Time Series im Blue and Fitted Auto Arima Model in Yellow.
plot(AR_dmd_APAC1$x, col="Blue", lwd=2)
lines(fitted(AR_dmd_APAC1), col="Yellow", lwd=2)

# Testing the Residual values , once we remove fitted auto arima.
resi_AR_dmd_APAC1 <- demand.APAC1$Monthly.Demand - fitted(AR_dmd_APAC1)
adf.test(resi_AR_dmd_APAC1, alternative = "stationary") #  p-value<0.01. 

kpss.test(resi_AR_dmd_APAC1)# p-value>0.1 #Residuals are stationary

hist(resi_AR_dmd_APAC1,probability = T)
lines(density(x = resi_AR_dmd_APAC1),col="red")
#Errors are also normally distributed
#This shows that log likelihood of classical decomposition model is higher than auto arima and coeff of variation 
#is almost the same between the 2 models.

#Let us forecast the Demand using for AR_dmd_APAC1 next 6 months 
Predict_dmd_APAC1<- predict(AR_dmd_APAC1, n.ahead = 6)
Predict_dmd_APAC1
check_accuracy1<- accuracy(Predict_dmd_APAC1$pred, test.demand.APAC1$Monthly.Demand)
check_accuracy1
mape_AR_dmd_APAC1<-check_accuracy1[5]
mape_AR_dmd_APAC1 #26.24458

#Mape from classical decomposition is better than auto arima.

#We will now plot 
final_demd_APAC1<- ts(c(fitted(AR_dmd_APAC1), Predict_dmd_APAC1$pred))
plot(ts(APAC1[,"Monthly.Demand"]), col="Green", lwd=2,main="APAC Consumer Demand Forecasting: Auto Arima")
lines(final_demd_APAC1, col="Red", lwd=2)


#####################################
#### NEXT: APAC_CONSUMER SALES ######
#####################################

CS_sales.APAC1<- sales.APAC1[(row_count-5):row_count,]#Test dataset
sales.APAC1<- sales.APAC1[1:(row_count-6),]#Training dataset

sales.APAC1.timeser<- ts(sales.APAC1[,"Monthly.Sales"])
sales.APAC1.timeser

# Lets see decomposed for 1 year
DC_sales.APAC1<- ts(sales.APAC1[,"Monthly.Sales"])
plot(DC_sales.APAC1)
DC_sales.APAC1<- decompose(DC_sales.APAC1)
DC_sales.APAC1
plot(DC_sales.APAC1)

#it's is seen to be linear trending upwards

#Sales Time Series
plot(sales.APAC1.timeser, col="Black", lwd=2)

#smoothening the time series
w <-1
Smooth_APAC_sales<-stats::filter(as.double(as.character(sales.APAC1.timeser)),
filter=rep(1/(2*w+1), (2*w+1)), method='convolution', sides=2)
length(Smooth_APAC_sales)#42

#Smoothing left end of the time series
diff <- Smooth_APAC_sales[w+2] - Smooth_APAC_sales[w+1]
for (i in seq(w,1,-1)) {
  Smooth_APAC_sales[i] <- Smooth_APAC_sales[i+1] - diff
}

#Smoothing right end of the time series

n <- length(sales.APAC1.timeser)
diff <- Smooth_APAC_sales[n-w] - Smooth_APAC_sales[n-w-1]
for (i in seq(n-w+1, n)) {
  Smooth_APAC_sales[i] <- Smooth_APAC_sales[i-1] + diff}

#Plot the smoothed time series

lines(Smooth_APAC_sales, col="Blue", lwd=2)
Smooth_APAC_sales_for_model<-as.data.frame(cbind(sales.APAC1["Month.number"],as.vector(Smooth_APAC_sales)))


colnames(Smooth_APAC_sales_for_model)[2]<-c('Monthly.Sales')

# We will model global trend aspect
#We are choosing a multiplicative model to train the data since it has a seasonal pattern with
#amplitude which is not constant and increases with time.
globaltrend_sales.APAC1 <- lm(Monthly.Sales~ sin(0.5*Month.number)*poly(Month.number,1)+
                                 cos(0.1*Month.number)*poly(Month.number,1)+
                                 tan(0.02*Month.number), data = Smooth_APAC_sales_for_model)
globaltrend_sales.APAC1

globcomp_sales.APAC1 <- predict(globaltrend_sales.APAC1, data=adj_sales.APAC1$Month.number)

lines(globcomp_sales.APAC1, col="Red", lwd=2)
#Monthly demand shown in Blue and FOrecasted Global Component in Yellow

#We have trend and sesonality.We will Isolate the Local Component and analyze its ARMA components
local_s_APAC1<- sales.APAC1$Monthly.Sales - globcomp_sales.APAC1
acf(local_s_APAC1,lag.max=40)
acf(local_s_APAC1,lag.max=40,type = "partial")
#Now let's plot this local component and analyze it graphically
plot(local_s_APAC1, col="Blue",type="l")

#We will now check if this local component can be modeled as AR(p) or MA(q) series.
local_arima_sales_APAC11<- auto.arima(local_s_APAC1)
local_arima_sales_APAC11  #ARIMA(0,0,0) with zero mean 
#sigma^2 estimated as 110440470:  log likelihood=-448.52
#AIC=899.03   AICc=899.13   BIC=900.77

#classical decomposition model for next 6 months [Month : 43,44,45,46,47,48].
Class_decom_sales.APAC1<- predict(globaltrend_sales.APAC1, data.frame(Month.number=CS_sales.APAC1$Month.number))
Class_decom_sales.APAC1

#We'll check if the residual series is white noise

resi_2 <- local_s_APAC1-fitted(local_arima_sales_APAC11)

adf.test(resi_2,alternative = "stationary")#Dickey-Fuller = -4.5817, Lag order = 3, p-value < 0.01
kpss.test(resi_2)#KPSS Level = 0.02957, Truncation lag parameter = 1, p-value > 0.1

#p-value of adf is < .05 and kpss p-value is > .1 which indicates that residual series is indeed white noise.
hist(resi_2,probability = T)
lines(density(resi_2),col="red") #Errors are also normally distributed
#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.APAC1 dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
ACC_CD_sales_APAC1<- accuracy(Class_decom_sales.APAC1, CS_sales.APAC1$Monthly.Sales)
ACC_CD_sales_APAC1
mape_clasdec_sales.APAC1<-ACC_CD_sales_APAC1[5]
mape_clasdec_sales.APAC1

#ME    RMSE      MAE      MPE     MAPE
#Test set 9800.081 17180.13 14600.94 10.34701 22.53589

# Demand Time Series im Green and Fitted Classical Decomposition in RED

full_clasdec_sales.APAC1<- ts(c(globcomp_sales.APAC1, Class_decom_sales.APAC1))
plot(ts(APAC1[,"Monthly.Sales"]), col="Green", lwd=2)
lines(full_clasdec_sales.APAC1, col="RED", lwd=2)

#looks like a good fit.

#We will check Auto Arima Model 
AR_sales_APAC1<- auto.arima(sales.APAC1$Monthly.Sales)
AR_sales_APAC1  # ARIMA(0,1,1) #sigma^2 estimated as 174361546:  log likelihood=-447.11
#AIC=898.23   AICc=898.55   BIC=901.66


# Demand Time Series im Blueand Fitted Auto Arima Model in Yellow.
plot(AR_sales_APAC1$x, col="Blue", lwd=2)
lines(fitted(AR_sales_APAC1), col="Yellow", lwd=2)

# Testing the Residual values , once we remove fitted auto arima.
resi_AR_sales_APAC1 <- sales.APAC1$Monthly.Sales - fitted(AR_sales_APAC1)
adf.test(resi_AR_sales_APAC1, alternative = "stationary") #  p-value <0.01. 
#Dickey-Fuller = -4.2563, Lag order = 3
kpss.test(resi_AR_sales_APAC1)# KPSS Level = 0.042734 and p-value >0.1 #Residuals are stationary
hist(resi_AR_sales_APAC1,probability = T)
lines(density(resi_AR_sales_APAC1),col="red") #Errors are also normally distributed
#This shows that log likelihood of classical decomposition model is just slightly higher than auto arima and coeff of variation 
#is almost the same between the 2 models.

#Let us forecast the Demand using for AR_dmd_APAC1 next 6 months 
Predict_sales_APAC1<- predict(AR_sales_APAC1, n.ahead = 12)
check_accuracy2<- accuracy(Predict_sales_APAC1$pred, CS_sales.APAC1$Monthly.Sales)
check_accuracy2
mape_AR_sales_APAC1<-check_accuracy2[5]
mape_AR_sales_APAC1 # 27.68952

#Mape in classical decomposition was 22.53589 is better than auto arima.


#We will now plot 
final_sales_APAC1<- ts(c(fitted(AR_sales_APAC1), Predict_sales_APAC1$pred))
plot(ts(APAC1[,"Monthly.Sales"]), col="Green", lwd=2)
lines(final_sales_APAC1, col="Red", lwd=2)

################################################
#### NEXT: EU_CONSUMER Demand of quantity ######
################################################

demand.EU1<- EU1[,c("Month.number","Monthly.Demand")]
sales.EU1<- EU1[,c("Month.number","Monthly.Sales")]
row_count<- nrow(EU1)

#We will build two demand forecasting models using the below techniques:
#We will use Classical Decomposition and Auto ARIMA

test.demand.EU1<- demand.EU1[(row_count-5):row_count,] #Test dataset
demand.EU1<- demand.EU1[1:(row_count-6),]  # Training dataset

TS_decom.demand.EU1<- ts(demand.EU1[,"Monthly.Demand"])
plot(TS_decom.demand.EU1)

length(TS_decom.demand.EU1)

w <-1
Smooth_EU1_demand<-stats::filter(TS_decom.demand.EU1,filter=rep(1/(2*w+1), (2*w+1)), method='convolution', sides=2)
length(Smooth_EU1_demand)
Smooth_EU1_demand

#Smoothing left end of the time series

diff <- Smooth_EU1_demand[w+2] - Smooth_EU1_demand[w+1]
for (i in seq(w,1,-1)) {
  Smooth_EU1_demand[i] <- Smooth_EU1_demand[i+1] - diff
}

#Smoothing right end of the time series

n <- length(TS_decom.demand.EU1)
diff <- Smooth_EU1_demand[n-w] - Smooth_EU1_demand[n-w-1]
for (i in seq(n-w+1, n)) {
  Smooth_EU1_demand[i] <- Smooth_EU1_demand[i-1] + diff}

#Plot the smoothed time series

plot(TS_decom.demand.EU1)
lines(Smooth_EU1_demand, col="Blue", lwd=2)


Smooth_EU1_demand_for_model<-as.data.frame(cbind(demand.EU1["Month.number"],as.vector(Smooth_EU1_demand)))
colnames(Smooth_EU1_demand_for_model)[2]<-c('Monthly.Demand')

#Building a model on the smoothed time series using classical decomposition

# We will model global trend aspect
# We are choosing a multiplicative model to train the data since it has a seasonal pattern with
# amplitude which is not constant and increases with time.

globaltrend_demand.EU1 <- lm(Monthly.Demand~ sin(0.5*Month.number)*poly(Month.number,1)+cos(0.1*Month.number)*poly(Month.number,1)+
                               tan(0.02*Month.number), data = Smooth_EU1_demand_for_model)

globcomp_demand.EU1 <- predict(globaltrend_demand.EU1, data=demand.EU1$Monthly.Demand)

summary(globcomp_demand.EU1)

lines(globcomp_demand.EU1, col="red",lwd=2)  

#accuracy(globcomp_demand.EU1, demand.EU1$Monthly.Demand)
#Monthly demand shown in Blue and Forecasted Global Component in Red


#We have trend and sesonality.We will Isolate the Local Component and analyze its ARMA components
local_d_EU1<- demand.EU1$Monthly.Demand - globcomp_demand.EU1

#Plotting local component for analyzing
plot(local_d_EU1, col="Blue",type ="l")

acf(local_d_EU1,lag.max=40)
acf(local_d_EU1, type="partial",lag.max=40)


#We will now check if this local component can be modeled as AR(p) or MA(q) series.
local_arima_demand_EU11<- auto.arima(local_d_EU1)
local_arima_demand_EU11
#ARIMA(0,0,0) with zero mean ;So AR(p) or MA(q) series in the local component

#classical decomposition model for next 6 months [Month : 43,44,45,46,47,48].
globaltrend_demand.EU1

Class_decom_demand.EU1<- predict(globaltrend_demand.EU1, data.frame(Month.number=test.demand.EU1$Month.number))
Class_decom_demand.EU1

#We'll check if the residual series is white noise

resi <- local_d_EU1-fitted(local_arima_demand_EU11)

adf.test(resi,alternative = "stationary") # Dickey-Fuller = -3.9032, Lag order = 3, p-value = 0.02305
kpss.test(resi) # KPSS Level = 0.023491, Truncation lag parameter = 1, p-value = 0.1


### p-value of adf is <.05 and p-value of kpss is =.1 indicates that residual series is indeed white noise.

#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.EU1 dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE

ACC_CD_demand_EU1<- accuracy(Class_decom_demand.EU1, test.demand.EU1$Monthly.Demand)
ACC_CD_demand_EU1
mape_clasdec_demand.EU1<-ACC_CD_demand_EU1[5]
mape_clasdec_demand.EU1   ###  MAPE Value 22.18186


# Demand Time Series im Green and Fitted Classical Decomposition in RED

full_clasdec_demand.EU1<- ts(c(globcomp_demand.EU1, Class_decom_demand.EU1))
plot(ts(EU1[,"Monthly.Demand"]), col="Green", lwd=2)
lines(full_clasdec_demand.EU1, col="RED", lwd=2)

#We will check Auto Arima Model 
AR_dmd_EU1<- auto.arima(demand.EU1$Monthly.Demand)
AR_dmd_EU1  # ARIMA(2,1,0)


# Demand Time Series im Blueand Fitted Auto Arima Model in Yellow.
plot(AR_dmd_EU1$x, col="Blue", lwd=2)
lines(fitted(AR_dmd_EU1), col="Yellow", lwd=2)


# Testing the Residual values , once we remove fitted auto arima.
resi_AR_dmd_EU1 <- demand.EU1$Monthly.Demand - fitted(AR_dmd_EU1)
adf.test(resi_AR_dmd_EU1, alternative = "stationary") #  Dickey-Fuller = -3.5969 p-value = 0.045211. 


kpss.test(resi_AR_dmd_EU1)# p-value = 0.1  ,KPSS Level = 0.047939

#This shows that log likelihood of classical decomposition model is higher than auto arima and coeff of variation 
#is almost the same between the 2 models.

#Let us forecast the Demand using for AR_dmd_EU1 next 6 months 
Predict_dmd_EU1<- predict(AR_dmd_EU1, n.ahead = 6)
Predict_dmd_EU1
check_accuracy1<- accuracy(Predict_dmd_EU1$pred, test.demand.EU1$Monthly.Demand)
check_accuracy1
mape_AR_dmd_EU1<-check_accuracy1[5]
mape_AR_dmd_EU1 #30.13319


#Mape from classical decomposition is better than auto arima.

#We will now plot 
final_demd_EU1<- ts(c(fitted(AR_dmd_EU1), Predict_dmd_EU1$pred))
plot(ts(EU1[,"Monthly.Demand"]), col="green", lwd=2)
lines(final_demd_EU1, col="Red", lwd=2)

################################################
#### NEXT: EU_CONSUMER SALES forecasting ######
################################################

CS_sales.EU1<- sales.EU1[(row_count-5):row_count,]#Test dataset
sales.EU1<- sales.EU1[1:(row_count-6),]#Training dataset


sales.EU1.timeser<- ts(sales.EU1[,"Monthly.Sales"])
sales.EU1.timeser

# Lets see decomposed for 1 year
DC_sales.EU1<- ts(sales.EU1[,"Monthly.Sales"])
plot(DC_sales.EU1)
DC_sales.EU1<- decompose(DC_sales.EU1)
DC_sales.EU1
plot(DC_sales.EU1)

#Sales Time Series
plot(sales.EU1.timeser, col="Black", lwd=1)

#smoothening the time series
w <-1
Smooth_EU1_sales<-stats::filter(as.double(as.character(sales.EU1.timeser)),
                                filter=rep(1/(2*w+1), (2*w+1)), method='convolution', sides=2)
length(Smooth_EU1_sales)#42

#Smoothing left end of the time series
diff <- Smooth_EU1_sales[w+2] - Smooth_EU1_sales[w+1]
for (i in seq(w,1,-1)) {
  Smooth_EU1_sales[i] <- Smooth_EU1_sales[i+1] - diff
}

#Smoothing right end of the time series

n <- length(sales.EU1.timeser)
diff <- Smooth_EU1_sales[n-w] - Smooth_EU1_sales[n-w-1]
for (i in seq(n-w+1, n)) {
  Smooth_EU1_sales[i] <- Smooth_EU1_sales[i-1] + diff}

lines(Smooth_EU1_sales, col="blue", lwd=2)
Smooth_EU1_sales_for_model<-as.data.frame(cbind(sales.EU1["Month.number"],as.vector(Smooth_EU1_sales)))
colnames(Smooth_EU1_sales_for_model)[2]<-c('Monthly.Sales')
# We will model global trend aspect
#We are choosing a multiplicative model to train the data since it has a seasonal pattern with
#amplitude which is not constant and increases with time.
globaltrend_sales.EU1 <- lm(Monthly.Sales~ sin(0.5*Month.number)*poly(Month.number,1)+
                              cos(0.1*Month.number)*poly(Month.number,1)+
                              tan(0.02*Month.number), data = Smooth_EU1_sales_for_model)
globaltrend_sales.EU1

globcomp_sales.EU1 <- predict(globaltrend_sales.EU1, data=adj_sales.EU1$Month.number)

lines(globcomp_sales.EU1, col="red", lwd=2)
#Monthly demand shown in Blue and FOrecasted Global Component in Yellow

#We have trend and sesonality.We will Isolate the Local Component and analyze its ARMA components
local_s_EU1<- sales.EU1$Monthly.Sales - globcomp_sales.EU1

#Now let's plot this local component and analyze it graphically
plot(local_s_EU1, col="Blue",type="l")

acf(local_s_EU1,lag.max=40)
acf(local_s_EU1,lag.max=40,type="partial")
#We will now check if this local component can be modeled as AR(p) or MA(q) series.
local_arima_sales_EU11<- auto.arima(local_s_EU1)
local_arima_sales_EU11  #ARIMA(0,0,0) with zero mean 
## sigma^2 estimated as 102582942:  log likelihood=-446.97
## AIC=895.93   AICc=896.03   BIC=897.67

#classical decomposition model for next 6 months [Month : 43,44,45,46,47,48].
Class_decom_sales.EU1<- predict(globaltrend_sales.EU1, data.frame(Month.number=CS_sales.EU1$Month.number))
Class_decom_sales.EU1

#We'll check if the residual series is white noise

resi_2 <- local_s_EU1-fitted(local_arima_sales_EU11)

adf.test(resi_2,alternative = "stationary")# Dickey-Fuller = -4.9744, Lag order = 3, p-value = 0.01
kpss.test(resi_2)# KPSS Level = 0.022045, Truncation lag parameter = 1, p-value = 0.1


#p-value of adf is < .05 and kpss p-value is > .1 which indicates that residual series is indeed white noise.

#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.EU1 dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
ACC_CD_sales_EU1<- accuracy(Class_decom_sales.EU1, CS_sales.EU1$Monthly.Sales)
ACC_CD_sales_EU1
mape_clasdec_sales.EU1<-ACC_CD_sales_EU1[5]
mape_clasdec_sales.EU1   ### 30.69751
# Demand Time Series im Green and Fitted Classical Decomposition in RED

full_clasdec_sales.EU1<- ts(c(globcomp_sales.EU1, Class_decom_sales.EU1))
plot(ts(EU1[,"Monthly.Sales"]), col="Green", lwd=2)
lines(full_clasdec_sales.EU1, col="RED", lwd=2)

#looks like a good fit.

#We will check Auto Arima Model 
AR_sales_EU1<- auto.arima(sales.EU1$Monthly.Sales)
AR_sales_EU1  # ARIMA(2,1,0) #sigma^2 estimated as 168564657:  log likelihood=-445.84
## AIC=897.67   AICc=898.32   BIC=902.8

# Demand Time Series im Blueand Fitted Auto Arima Model in Yellow.
plot(AR_sales_EU1$x, col="Blue", lwd=2)
lines(fitted(AR_sales_EU1), col="Yellow", lwd=2)

# Testing the Residual values , once we remove fitted auto arima.
resi_AR_sales_EU1 <- sales.EU1$Monthly.Sales - fitted(AR_sales_EU1)

adf.test(resi_AR_sales_EU1, alternative = "stationary") # Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01

kpss.test(resi_AR_sales_EU1)# KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1
#This shows that log likelihood of classical decomposition model is just slightly higher than auto arima and coeff of variation 
#is almost the same between the 2 models.

#Let us forecast the Demand using for AR_dmd_EU1 next 6 months 
Predict_sales_EU1<- predict(AR_sales_EU1, n.ahead = 6)
Predict_sales_EU1
check_accuracy2<- accuracy(Predict_sales_EU1$pred, CS_sales.EU1$Monthly.Sales)
check_accuracy2
mape_AR_sales_EU1<-check_accuracy2[5]
mape_AR_sales_EU1 # 28.9226

#Mape in Auto Arima was 28.9226 and it is better than classical decomposition


#We will now plot 
final_sales_EU1<- ts(c(fitted(AR_sales_EU1), Predict_sales_EU1$pred))
plot(ts(EU1[,"Monthly.Sales"]), col="Green", lwd=2)
lines(final_sales_EU1, col="Red", lwd=2)

