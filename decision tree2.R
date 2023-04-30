#set time zone
Sys.setenv(TZ="UTC")
Sys.setlocale("LC_TIME","English")

library(ggplot2)
library(rpart)
library(rpart.plot)
#install.packages('caret')
library(caret)
df <- read.csv('b2bForR.csv')
df$Number.of.Employees <- as.numeric(df$Number.of.Employees)
df$Sector <- as.factor(df$Sector)
df$Stage <- as.factor(df$Stage)
df$HQ_Country <- as.factor(df$HQ_Country)
df$Angel_. <- as.factor(df$Angel_.)
df$isActive <- as.factor(df$isActive)
df$SubSector <- as.factor(df$SubSector)
class(df$Number.of.Employees)
summary(df)
filter <- sample.split(df$isActive,SplitRatio = 0.8)
df.train <- subset(df,filter ==T)  
df.test <- subset(df,filter ==F) 

ggplot(df, aes(Sector, fill = Total_Raised)) + geom_bar()

model.dt <- rpart(isActive~ Company_Age + Number.of.Employees+Funds.Round+Angel_.+ Sector+Stage +HQ_Country+Total_Raised+SubSector,df.train)
model.dt2 <- rpart(Stage~ Company_Age + Number.of.Employees+Funds.Round+Angel_.+ Sector+Stage +HQ_Country+Total_Raised+SubSector+isActive,df.train)
rpart.plot(model.dt, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#run algorithm to build model
df.model <- glm (isActive~ Company_Age + Number.of.Employees+Funds.Round+Stage+Total_Raised ,family = binomial(link = "logit"),data = df.train)
summary(df.model)
predict.test <- predict(df.model,newdata = df.test,type = "response")
cfNB <- table(predict.test > 0.5,df.test$isActive )

accuracy <- sum(diag(cfNB)) / sum(cfNB)







plot(model)
text(model)
set.seed(123)
index <- createDataPartition(df$isActive, p = 0.7, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

str(df)
pred <- predict(model, newdata = test_data, type = "class")

pred <- na.omit(pred)
test_labels <- na.omit(test_data$isActive)
confusion_matrix <- table(Actual_value = test_labels, Predicted_value = pred)

confusion_matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

