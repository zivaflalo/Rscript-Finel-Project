
library(ggplot2)




df <- read.csv('b2bForR.csv')
str(df)
df$Company_Status <- as.factor(df$Company_Status)
df$Stage <- as.factor(df$Stage)


ggplot(df, aes(df$Company_Age,df$Total_Raised))+
  geom_point(size = 3)
ggplot(df,aes(Company_Age,Total_Raised,
             colour = isActive))+
geom_point(size = 3, alpha = 0.5)+
geom_smooth(method = lm, se =F)+
facet_wrap(~Sector)

  




