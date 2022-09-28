#Step 1: importing csv files
politicalLeanings <- read.csv("Lucca's Political Leanings in Cities in the USA - Sheet1.csv")
crimeRates <- read.csv("Lucca's Crime Rates by Metropolitan Areas 2015 - Crime Rates.csv")

#Step 2: selecting only which rows I want to use in the csv files I imported
#politicalLeanings already has only the columns which I need.
crimeRates <- subset(crimeRates, select = c(department_name, total_pop, violent_per_100k))

#Step 3: Ridding Cities that have missing values in their datasets
#politicalLeanings has no missing data.
crimeRates <- subset(crimeRates, violent_per_100k != "N/A")
crimeRates <- subset(crimeRates, total_pop != "N/A")

#Step 4: renaming the columns I am using for consistency in the modified csv 
colnames(politicalLeanings) <- c("Cities","Policy.Preferences")
colnames(crimeRates) <- c("Cities","Total.Pop","Violent.Crime")

#Step 5: merging both files
merge <- merge(politicalLeanings, crimeRates, by="Cities", all=TRUE)
merge <- na.omit(merge)

#Step 6: viewing merge file.
View(merge)

#Step 7: Creating Box Plots for one-variable analysis
boxplot(merge$Policy.Preferences,
        main="Boxplot of Distribution of Policy Preferences in 53 Cities in the U.S.A. (2016)",
        horizontal=TRUE, 
        xlab="Policy Preferences (-1 = most liberal, 1 = most conservative)", 
        ylim=c(-1,1))

boxplot(merge$Violent.Crime,
        main="Boxplot of Distribution of Violent Crime in 53 Cities in the U.S.A. (2015)",
        horizontal=TRUE, 
        xlab="Violent Crime Rate per 100,000 People")

#Step 8: Creating Histograms for one-variable analysis
hist(merge$Policy.Preferences,
     main="Histogram of Distribution of Policy Preferences in 53 Cities in the U.S.A. (2016)",
     xlab="Leanings towards Policy Preferences (-1 = most liberal, 1 = most conservative)",
     ylab="Number of Cities",
     col="yellow",
     border="black", 
     xlim=c(-1,1))

hist(merge$Violent.Crime,
     main="Histogram of Distribution of Violent Crime in 53 Cities in the U.S.A. (2015)",
     xlab="Violent Crime Rate per 100,000 People",
     ylab="Number of Cities",
     col="yellow",
     border="black")

#Step 9: Calculations for the Histogram and Boxplot
summary(merge$Policy.Preferences)
summary(merge$Violent.Crime)
sd(merge$Policy.Preferences)
sd(merge$Violent.Crime)
IQR(merge$Policy.Preferences)
IQR(merge$Violent.Crime)
range(merge$Policy.Preferences)
range(merge$Violent.Crime)

#Step 10: Creating Scatterplots for two-variable analysis
plot(merge$Policy.Preferences, merge$Violent.Crime,
     xlab="Leanings towards Policy Preferences (-1 = most liberal, 1 = most conservative)", ylab="Violent Crime Rate per 100,000 People",
     main="Scatterplot: Policy Preferences vs. Violent Crime Rate")

model1 <- lm(merge$Violent.Crime~merge$Policy.Preferences)
abline(model1)
summary(model1)

model1_res <- resid(model1)
plot(merge$Policy.Preferences, model1_res,
     ylab="Residuals", xlab="Leanings towards Policy Preferences (-1 = most liberal, 1 = most conservative)",
     main="Residual Plot: Policy Preferences vs. Violent Crime Rate")
abline(0,0)


