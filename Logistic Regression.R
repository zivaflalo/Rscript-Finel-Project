#install.packages('caTools')
library(caTools)

df <- read.csv('b2bForR.csv')
str(df)
df$Number.of.Employees <- as.numeric(df$Number.of.Employees)
class(df$Number.of.Employees)
split <- sample.split(df,SplitRatio = 0.7)
train <- subset(df, split == "TRUE")
test <- subset(df, split == "FALSE")
model <- glm(isActive ~ Company_Age +Funds.Round,data = train, family = 'binomial')
summary(model)
res <- predict(model, test,type="response")
res
res <- predict(model, train,type="response")
res
confmatrix <- table(Actual_value=train$isActive,predicted_value = res>0.5)
confmatrix
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)
