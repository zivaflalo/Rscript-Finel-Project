library(ggplot2)
#install.packages('plotly')
library(plotly)

df <- read.csv('b2bForR.csv')
df$Number.of.Employees <- as.numeric(df$Number.of.Employees)


hist(df$Company_Age, breaks = 70, col = "red")     
hist(df$Total_Raised, breaks = 70, col = "red")     
hist(df$Total_Raised, breaks = 130, col = "red",xlim = c(1, 70), ylim = c(1, 90)) 
mean(df$Total_Raised)
barplot(df$Total_Raised, names.arg = rownames(df$Company_Age), 
        xlab = "df$Company_Age", ylab = "df$Total_Raised")
ggplot(data = df, aes(x = df$Company_Age, y = df$Total_Raised)) +
  geom_point()
boxplot(df$Company_Age)
plot(df$Total_Raised, df$Company_Age,type = "l")
barplot(df$Company_Age, names.arg = df$Company_Status)
ggplot(df, aes(x = df$Company_Age, y = df$Total_Raised, color = df$isActive)) + 
  geom_point()
plot_ly(df, x = ~df$Company_Age, y = ~df$Total_Raised, z = ~df$Company_Status, type = "scatter3d", mode = "markers")
ggplot(df, aes(x = Company_Age, y = Total_Raised, fill = Company_Status)) +
  geom_bar(stat = "identity")
