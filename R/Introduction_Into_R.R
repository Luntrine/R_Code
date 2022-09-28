#importing and reading csv file into the workspace
analytics1 <- read.csv("Analytics1.csv")
analytics2 <- read.csv("Analytics2.csv")

View(analytics1)
View(analytics2)

analytics <- merge(analytics1, analytics2, by="Country")

correlation <- subset(analytics, select = c(Country, Mobile, Happiness.Score))
correlation <- na.omit(correlation)

plot(correlation$Mobile, correlation$Happiness.Score,
     xlab="Mobile Subscription per 100 people", ylab="Happiness Score",
     main="Scatterplot: Mobile Subscription and Happiness Score")

model1 <- lm(correlation$Happiness.Score~correlation$Mobile)
abline(model1)

model1_res <- resid(model1)
plot(correlation$Mobile, model1_res,
     ylab="Residuals", xlab="Mobile Subscription per 100 People",
     main="Residual Plot: Mobile Subscription and Happiness Score")
abline(0,0)




View(analytics)

boxplot(analytics$Homicide,
        main="Distribution of Homicide Rate",
        horizontal=TRUE, xlab="Homicide Rate")

hist(analytics$Homicide,
     main="Histogram of Homocide Rates",
     xlab="Homocide Rate",
     ylab="Number of countries",
     col="yellow",
     border="black")


mean(analytics$Homicide)
sd(analytics$Homicide)
summary(analytics$Homicide)
mean(analytics$Homicide)
sd(analytics$Homicide)

