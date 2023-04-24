#set time zone
Sys.setenv(TZ="UTC")
Sys.setlocale("LC_TIME","English")

#set the working directory
setwd("~/תעשייה וניהול/שנה ג סמסטר ב/כריית ידע ולמידת מכונה/data_R")

#read raw data
stroke.data.raw <- read.csv("stroke.csv")
stroke.data.prepered<-stroke.data.raw

#see structure
str(stroke.data.prepered)

#remove ID attribute - will not help to model
stroke.data.prepered$id=NULL

#analyze gender attribue
table(stroke.data.prepered$gender)
#set other to the majority class
makeFemale<- function(x)
{
  if (x=="Other") return ("Female")
  return (x)
}
stroke.data.prepered$gender<- sapply(stroke.data.prepered$gender,makeFemale)
#set gender to factor
stroke.data.prepered$gender<-as.factor(stroke.data.prepered$gender)
table(stroke.data.prepered$gender)

#analyze avg_glucose_level
summary(stroke.data.prepered$avg_glucose_level)

#analyze bmi
table(stroke.data.prepered$bmi)
summary(stroke.data.prepered$bmi)
#convert from chr to numeric type
#remove na's from bmi
makeNaEvarage <- function(x,vec){
  if(is.na(x)) return (mean(vec,na.rm=T))
  return (x)
}
stroke.data.prepered$bmi<- as.numeric(stroke.data.prepered$bmi)
stroke.data.prepered$bmi<-sapply(stroke.data.prepered$bmi,makeNaEvarage,vec=stroke.data.prepered$bmi)
summary(stroke.data.prepered$bmi)

##analyze stroke
table(stroke.data.prepered$stroke)
makeStrokeYesNo <- function(x){
  if(x==1) return ("Yes") 
  return ("No")
  
}
stroke.data.prepered$stroke<-sapply(stroke.data.prepered$stroke,makeStrokeYesNo)
stroke.data.prepered$stroke<-as.factor(stroke.data.prepered$stroke)
table(stroke.data.prepered$stroke)


#set work_type to factor
stroke.data.prepered$work_type<-as.factor(stroke.data.prepered$work_type)

#set Residence_type to factor
stroke.data.prepered$Residence_type<-as.factor(stroke.data.prepered$Residence_type)

#set ever_married to factor
stroke.data.prepered$ever_married<-as.factor(stroke.data.prepered$ever_married)

#set smoking_status to factor
stroke.data.prepered$smoking_status<-as.factor(stroke.data.prepered$smoking_status)

#over the target attribute
library(ROSE)
table(stroke.data.prepered$stroke)
stroke.data.prepered <- ovun.sample(stroke~.,data = stroke.data.prepered,method = 'over',N=7000)$data
table(stroke.data.prepered$stroke)


library(ggplot2)

ggplot(stroke.data.prepered, aes(gender, fill = stroke)) + geom_bar()
ggplot(stroke.data.prepered, aes(gender, fill = stroke)) + geom_bar(position = 'fill')

ggplot(stroke.data.prepered, aes(age, fill = stroke)) + geom_bar()

ggplot(data = stroke.data.prepered, aes(x=age))+geom_histogram(binwidth = 20)
ggplot(stroke.data.prepered, aes(stroke, fill=stroke))+geom_bar()

ggplot(stroke.data.prepered, aes(smoking_status, fill = stroke)) + geom_bar()
ggplot(stroke.data.prepered, aes(smoking_status, fill = stroke)) + geom_bar(position = 'fill')
ggplot(stroke.data.prepered,aes(stroke,avg_glucose_level))+geom_boxplot()
ggplot(stroke.data.prepered,aes(stroke,age))+geom_boxplot()

#see ever_married prediction
cfEM <- table(stroke.data.prepered$ever_married, stroke.data.prepered$stroke)

precisionEM <- cfEM['Yes','Yes']/(cfEM['Yes','Yes'] + cfEM['Yes','No'] )
recallEM <- cfEM['Yes','Yes']/(cfEM['Yes','Yes'] + cfEM['No','Yes'] )

(cfEM['Yes','No']+ cfEM['No','Yes'])/dim( stroke.data.prepered)[1]

#build logistic reressrion model
library(caTools)

#split to train set and test set
filter <- sample.split(stroke.data.prepered$stroke,SplitRatio = 0.7)
stroke.train <- subset(stroke.data.prepered,filter ==T)  
stroke.test <- subset(stroke.data.prepered,filter ==F) 

#check for correct dimension
dim(stroke.train)
dim(stroke.test)

#run algorithm to build model
stroke.model <- glm (stroke~age+avg_glucose_level+bmi ,family = binomial(link = "logit"),data = stroke.train)
summary(stroke.model)

#make the prediction according to the test set
predict.test <- predict(stroke.model,newdata = stroke.test,type = "response")

#build the confuision marix
cfNB <- table(predict.test > 0.5,stroke.test$stroke )

#calculate presition and recall
precision <-cfNB['TRUE','Yes']/(cfNB['TRUE','Yes'] + cfNB['TRUE','No'] )
recall <- cfNB['TRUE','Yes']/(cfNB['TRUE','Yes'] + cfNB['FALSE','Yes'] )



#build desition tree model
library(rpart)
library(rpart.plot)


model.dt <- rpart(stroke~age+avg_glucose_level+bmi ,stroke.train)
rpart.plot(model.dt, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#calculating the confusion matrix
predict.test.dt <- predict(model.dt,stroke.test)
predict.test.dt <- predict.test.dt[,'Yes']

#build the confuision marix
cfDT <- table(predict.test.dt > 0.5,stroke.test$stroke )

#calculate presition and recall
precisionDT <-cfDT['TRUE','Yes']/(cfDT['TRUE','Yes'] + cfDT['TRUE','No'] )
recallDT <- cfDT['TRUE','Yes']/(cfDT['TRUE','Yes'] + cfDT['FALSE','Yes'] )



#run algorithm to build model with all the attribute
stroke.model.all <- glm (stroke~. ,family = binomial(link = "logit"),data = stroke.train)
summary(stroke.model.all)

#make the prediction according to the test set
predict.test.all <- predict(stroke.model.all,newdata = stroke.test,type = "response")

#build the confuision marix
cfAL <- table(predict.test.all > 0.45,stroke.test$stroke )

#calculate presition and recall
precision.all <-cfAL['TRUE','Yes']/(cfAL['TRUE','Yes'] + cfAL['TRUE','No'] )
recall.all <- cfAL['TRUE','Yes']/(cfAL['TRUE','Yes'] + cfAL['FALSE','Yes'] )



#ROC curve 
library(pROC)

rocCurveLR <- roc(stroke.test$stroke, predict.test, direction = "<", levels = c("No","Yes"))
rocCurveDT <- roc(stroke.test$stroke, predict.test.dt, direction = "<", levels = c("No","Yes"))
rocCurveAL <- roc(stroke.test$stroke, predict.test.all, direction = "<", levels = c("No","Yes"))

#Calculate AUC
auc(rocCurveDT)
auc(rocCurveLR)



plot(rocCurveDT, col="red", main='ROC chart')
par(new=TRUE)
plot(rocCurveLR, col="blue", main='ROC chart')
par(new=TRUE)
plot(rocCurveAL, col="pink", main='ROC chart')

