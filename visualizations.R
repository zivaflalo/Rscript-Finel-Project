#set time zone
Sys.setenv(TZ="UTC")
Sys.setlocale("LC_TIME","English")
library(GGally)
library(ggplot2)
#install.packages('plotly')
library(plotly)
library(reshape2)

df <- read.csv('b2bForR.csv')
df$Number.of.Employees <- as.numeric(df$Number.of.Employees)
df$Sector <- as.factor(df$Sector)
df$Stage <- as.factor(df$Stage)
df$HQ_Country <- as.factor(df$HQ_Country)
df$Angel_. <- as.factor(df$Angel_.)
df$isActive <- as.factor(df$isActive)
df$SubSector <- as.factor(df$SubSector)
df$Platform_Category <- as.factor(df$Platform_Category)


hist(df$Company_Age, breaks = 70, col = "red") 
hist(df$Total_Raised, breaks = 70, col = "red")     
hist(df$Total_Raised, breaks = 130, col = "green",xlim = c(1, 70), ylim = c(1, 90)) 
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

ggplot(df, aes(x=Total_Raised,fill = Company_Age)) +
  geom_bar()

melted_data <- melt(df, id.vars = "Company_Age")
ggplot(melted_data, aes(x=Total_Raised, y=Company_Age, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="white", high="blue") +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
  labs(title="Heatmap of mtcars Dataset", x="", y="Number of Cylinders", fill="Value")
       
       