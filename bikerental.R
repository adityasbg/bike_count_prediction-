setwd('/home/aditya/code_pen/edwiser_pro_r')
getwd()
rm(list = ls())
data = read.csv("day.csv")
head(data)
#droping instant variable as it  Record index 
data$instant=NULL
head(data)

  #data frame description 
str(data)
summary(data)

#data visualisation 

temp =data

categorical_var =c("season","yr","mnth","holiday","weekday","workingday","weathersit")
#converting all categorical variable to character type
for ( i in categorical_var){
  temp[,i] =as.character(temp[,i] )
  
}
# converting dteday to Date format 
temp$dteday =as.Date(temp$dteday)
rownames(temp) =temp$dteday
# droping dteday after setting it as index 
temp$dteday =NULL

temp$season[temp$season %in%  1]="Spring"
temp$season[temp$season %in%  2]="Summer"
temp$season[temp$season %in%  3]="Fall"
temp$season[temp$season %in%  4]="Winter"

temp$yr[temp$yr %in% 0]="2011"
temp$yr[temp$yr %in% 1]="2012"

temp$holiday[temp$holiday  %in% 0]="Working day"
temp$holiday[temp$holiday  %in% 1]="Holiday"

temp$weathersit[temp$weathersit %in% 1]="Clear"
temp$weathersit[temp$weathersit %in% 2]="Cloudy/Mist"
temp$weathersit[temp$weathersit %in% 3]="Rain/Snow/Fog"
temp$weathersit[temp$weathersit %in% 4]="Heavy/Rain/Snow/Fog"

temp$temp =temp$temp *39
temp$atemp =temp$atemp *50
temp$windspeed =temp$windspeed *67
temp$hum =temp$hum *100

head(temp)

#pair plot for numeric variable analysis 
continous=c('temp', 'atemp', 'hum', 'windspeed','casual','registered', 'cnt')
par(mar=c(1,1,1,1))
library("ggplot2")                     # Load ggplot2 package
library("GGally")                      # Load GGally package
  ggpairs(temp[continous])

# Observation
#1. temp and atemp highly posetively correlated 
#2. cnt and registered is highly positevely  correlated 
#3. cnt and casual is positevely correlated 
#4. casual is right skewed 
#5. atemp is shows a moderate corelation toward cnt 
#6. temp is shows a moderate corelation toward cnt 

category =c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday',
           'weathersit')

j=1
for( i in category){
assign(paste0("gn",j) ,ggplot(data =temp ,aes_string(x=i ,fill=i) )+
  geom_bar(stat ="count",width = 0.5 ) +
  theme_minimal()
)
j=j+1
}
gridExtra::grid.arrange(gn1,gn2,gn4,gn5,gn6,gn7,nrow=3 ,ncol=3)
gridExtra::grid.arrange(gn3 ,nrow=2 ,ncol=2)

# Observation 
#1. number of holiday was less then working day 
#2. there are very  few observation of Rain/snow/fog



library(dplyr)
#data summarisation with respect to target variable

cntByYear=temp %>% 
  group_by(yr) %>%
  summarise(cnt=sum(cnt))

assign(paste0("tp",1),ggplot(data =cntByYear ,aes(x=yr ,y=cnt,fill=yr))+
  geom_col()+
  theme_minimal()+
  ggtitle("Number of bike rented per year ")
)

preSeasonCnt =temp %>% 
  group_by(season) %>%
  summarise(cnt =sum(cnt))

assign(paste0("tp",2),ggplot(data =preSeasonCnt ,aes(x=season ,y=cnt,fill=season))+
  geom_col()+
  theme_minimal()+
  ggtitle("Number of bike rented per season ")
)

preMnthCnt =temp %>% 
  group_by(mnth) %>%
  summarise(cnt =sum(cnt))

assign(paste0("tp",3),ggplot(data =preMnthCnt ,aes(x=mnth ,y=cnt,fill=mnth))+
  geom_col()+
  theme_minimal()+
  ggtitle("Number of bike rented in given month  ")
)

holidayCnt =temp %>% 
  group_by(holiday) %>%
  summarise(cnt =sum(cnt))

assign(paste0("tp",4),ggplot(data =holidayCnt ,aes(x=holiday ,y=cnt,fill=holiday))+
  geom_col()+
  theme_minimal()+
  ggtitle("Number of bike rented on working and non working days  ")
)


weekdayCnt =temp %>% 
  group_by(weekday) %>%
  summarise(cnt =sum(cnt))

assign(paste0("tp",5),ggplot(data =weekdayCnt  ,aes(x=weekday ,y=cnt,fill=weekday))+
  geom_col()+
  theme_minimal()+
  ggtitle("Number of bike rented on per weekday ")
)


weathersitCnt =temp %>% 
  group_by(weathersit) %>%
  summarise(cnt =sum(cnt))

assign(paste0("tp",6),ggplot(data =weathersitCnt  ,aes(x=weathersit ,y=cnt,fill=weathersit))+
  geom_col()+
  theme_minimal()+
  ggtitle("Number of bike rented given weather situation  ")
)


workingdayCnt =temp %>% 
  group_by(workingday) %>%
  summarise(cnt =sum(cnt))

assign(paste0("tp",7),ggplot(data =workingdayCnt  ,aes(x=workingday ,y=cnt,fill=workingday))+
  geom_col()+
  theme_minimal()+
  ggtitle("Number of bike rented given it is working day or not  ")
)

gridExtra::grid.arrange(tp1,tp2 ,tp3,tp7 ,ncol=2,nrow=2)
gridExtra::grid.arrange(tp4,tp5,tp6)
# Observations
#1. On year 2012  more user rented bike  2011
#2. On Fall season more number of people rented bike
#3. On month 8 or August most no of bike where rented  amount =351194
#4. On month 1 or january least number of bike where rented amoumt = 134933
#5. On working day most number of bike where rented amount =3214244
#6. On weekday 5  or friday most number of bike were reneted amount = 487790
#7. On weekday 0 or sunday least number of bike where rented  amount =444027
#8. On clear weather most number of bike was rented amount =2257952
#9. On Rain/snow/fog   least number of bike was rented amount =37869
#10. On holiday very few number of bike where rented compared to working day


#Exploratory data analysis

#dteday 

data$dteday  = as.Date(data$dteday)
# date type can be split into day ,year ,months , weekday  .we all ready have year , months , weekdays 
#creating dummy day variable
data$day =NA

#now we will extract day from date
for(i in 1:dim(data)[1]){
  data$day[i] = unclass(as.POSIXlt(data$dteday[i]))$mday
}

#setting date as index  
rownames(data) =data$dteday
data$dteday=NULL

#season: Season (1:springer, 2:summer, 3:fall, 4:winter)
data$season = as.character(data$season)

#yr: Year (0: 2011, 1:2012)
data$yr = as.character(data$yr)

#mnth: Month (1 to 12)
data$mnth =as.character(data$mnth)

#holiday: weather day is holiday or not (extracted fromHoliday Schedule)
data$holiday =as.character(data$holiday)

#weekday: Day of the week [0 -6]
data$weekday =as.character(data$weekday)

#workingday: If day is neither weekend nor holiday is 1, otherwise is 0.

data$workingday =as.character(data$workingday)

#weathersit: (extracted fromFreemeteo)
#1: Clear, Few clouds, Partly cloudy, Partly cloudy
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered
#clouds
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

# After checking the weather situation it was found that there is no category 4 in data 
data$weathersit =as.character(data$weathersit)
# day of month
data$day = as.numeric(data$day)

str(data)
temp =data 

#missing value analysis
missing_col = data.frame(apply(data ,2 ,function(x){sum(is.na(x))}))
colnames(missing_col)[1] ="percentage"
missing_col$percentage =(missing_col$percentage /nrow(data))
missing_col$percentage  =missing_col[order(-missing_col$percentage),]
#zero missing value 


# outlier analysis 
# on continous variable 
numeric_col=c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered',
             'day')


#creating box plot for numeric variables  
for(i in 1:length(numeric_col)){
assign(paste0("bp",i),ggplot(data =data ,aes_string(y=numeric_col[i])) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot( notch = FALSE , outlier.size=1 ,notchwidth = .2,outlier.colour = "red" ,fill="blue")+
    labs(y=numeric_col[i])+
    ggtitle(paste("Box plot of ",numeric_col[i]))
)
}

#ploting boxplot 
gridExtra::grid.arrange(bp1 ,bp2,bp3,bp4,bp5,bp6,bp7 ,ncol=3, nrow=3)
          # We need to remove outlier from casual ,hum,wind speed



#removing outlier from numerical variables
for(i  in numeric_col ){
  print(i)
#fetching outlier values
val=data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
print(length(val))
# filling outliers with na 
data[,i][data[,i] %in% val]=NA

}

# missing value after outlier removal 
colSums(is.na(data))

#knn imputing value as mean and median are giving poor results 
library(DMwR)

#coverting categorical value to number, not as data is all ready  encoded
categorical_var =c("season","yr","mnth","holiday","weekday","workingday","weathersit")

for(i in categorical_var){
  data[,i]=as.numeric(data[,i])
}
#knn imputation
data=knnImputation(data ,k=5)
# imputated 0 missing value 
colSums(is.na(data))

# correlation plot
numeric_col=c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered',
              'day' ,'cnt')
numericData=data[numeric_col]
library(corrgram)
#correlation plot 
corrgram(numericData ,upper.panel=panel.pie ,diag.panel=panel.density,text.panel = panel.txt ,main="Correlation Plot")
# temp is positevely correlated with atemp we should drop atemp
# removing atemp  from data 
data$atemp =NULL

colnames(data)

#Feature scaling 
#Normality check
# ploting distribution plot 

  j=1
  for( i in numeric_col ){
  assign(paste0("dp",j),ggplot(numericData , aes_string(x=i))+
    geom_density(fill="palegreen3")+
    theme_minimal()+
    ggtitle(paste("Distribution plot of ", i)) )
    j=j+1
  }

gridExtra::grid.arrange(dp1,dp2,dp3 ,dp4 ,dp5 ,dp6,dp7 ,dp8 ,ncol=3,nrow=3)
#most of the distribution are not normal 

#humidity and wind speed is some what normal
numeric_var =c('temp' ,'hum','windspeed','casual','registered'  ,'day')
#normalising data

temp =data
#normalising data
for(i in numeric_var ){
  print(i)
 data[,i]=(data[,i] -max(data[,i])) / (min(data[,i]) - max(data[,i]))
}


#Model development 

# Decision tree 
# Random forest 
# Linear regression 




library(DataCombine)
library(caret)
set.seed(123)

#Simple random sampling
train.index =sample(1:nrow(data) ,.8 *nrow(data))
train = data[train.index,]
test =data[-train.index,]

#dimension of test train data 
dim(train)
dim(test)

##Decision tree for classification
#Develop Model on training data
library(rpart)
library(MASS)
#Train Decision tree 
dtmodel=  rpart(cnt~., data =train ,method ='anova')
#Prediction on test data
prediction_DT =predict(dtmodel ,test[-13])
library("rpart.plot")
rpart.plot(dtmodel,box.palette="RdBu", shadow.col="gray", nn=TRUE)
#MAPE function 
MAPE =function (act ,pred){
    mean(abs((act-pred)/act)) *100
}
#Test MAPE =10.6%
#Accuracy 89.4%
#MAPE score on test data 
MAPE(test[,13] ,prediction_DT)

#Model evalution with  multiple error metric 
regr.eval(trues = test[,13], preds = prediction_DT, stats = c("mae","mse","rmse","mape" ))
#mae          mse         rmse         mape 
#4.116334e+02 2.668836e+05 5.166077e+02 1.061794e-01 

#actual vs predicted plot 
plot(test[,13],type = 'p',col="blue" ,main = "Actual vs predicted plot blue(Actual) red(Predicted) Decision Tree" )
lines(prediction_DT ,col='red')

######################### Random forest ##################################

library(randomForest)
#train  Random Forest 
rf_model =randomForest(cnt~. , train , importance=TRUE ,ntree=50)
#predict test data 
pred_y =predict(rf_model , test[-13])
#MAPE test data 4.7%   ntree =50  , increasing ntree doesnt improve the model
#inceasing trees may over train model
#accuracy test 95.3%
#MAPE score on test data 
MAPE(test[,13],  pred_y)

#model evalution with  multiple error metric 
regr.eval(test[,13] , preds = pred_y, stats = c("mae","mse","rmse","mape"))
#mae          mse         rmse         mape 
#1.749346e+02 6.959832e+04 2.638149e+02 4.797810e-02

#actual vs predicted plot 
plot(test[,13],type = 'p',col="blue" ,main = "Actual vs predicted plot blue(Actual) red(Predicted) Random forest", )
lines(pred_y ,col='red')

###################### Linear Regression #########################
  #MAPE test =3.07%
#accuracy on test data 97% 
#training linear regression
linearmodel =lm(cnt~.,data =train )
#model summary
summary(linearmodel)
#prediction on test data
y_pred = predict(linearmodel ,test[-13])

#MAPE score on test data 
MAPE(test[,13],y_pred)

#model evalution with  multiple error metric 
regr.eval(trues = test[,13], preds = y_pred, stats = c("mae","mse","rmse","mape"))
#mae          mse         rmse         mape 
#1.227498e+02 5.044234e+04 2.245937e+02 3.070828e-02 

#actual vs predicted plot
plot(test[,13],type = 'p',col="blue" ,main = "Actual vs predicted plot blue(Actual) red(Predicted) Linear Regression", )
lines(y_pred ,col='red')


# Overall Linear regression is best model compared to others 
# Linear regression gives best accuracy and low error rate 
# Accuracy =97% in test data 
# MAPE test =3.07%