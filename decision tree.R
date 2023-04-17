library(rpart)
#install.packages('caret')
library(caret)
df <- read.csv('b2bForR.csv')
df$Number.of.Employees <- as.numeric(df$Number.of.Employees)
class(df$Number.of.Employees)

model <- rpart(isActive ~ Company_Age +Funds.Round+Number.of.Employees+Total_Raised+Management_Members+Angel_.+Num_Sentences ,data = train_data,method = "class")
model
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

