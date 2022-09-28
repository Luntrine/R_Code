#1. Reading CSV files from the World Bank about Fertility Rate and Country Code.
fertilityrate <- read.csv("FertilityRate.csv")
country <- read.csv("CountryCode.csv")

#2. Cleaning up the Country Code Dataset
#a. selecting only the columns "Country.Code" and "Region" and overwriting the old country dataset with this one.
country <- subset(country, select = c(Country.Code,Region))
#b. selecting only the rows where Region exists and overwriting the old country dataset with this one.
country <- subset(country, Region != "")
#c. renaming the column names as "Country.Code" and "World.Region" and "Income.Group"
colnames(country) <- c("Country.Code","World.Region")

#3. Cleaning up the Fertility Rate Dataset by selecting only the columns "Country.Code", "Region", as well as the years 1980 and 2018, as well as overwriting the old country dataset with this one.
fertilityrate <- subset(fertilityrate, select = c(Country.Name,Country.Code,X1980,X2018))

#4. Merge the Country Codes dataset with the Fertility Rate Dataset by the column "Country.Code".
fertilityrate_country <- merge(fertilityrate, country, by="Country.Code")

#5. Cleaning up the Merge(fertilityrate_country) dataset by omiting rows with missing values.
fertilityrate_country <- subset(fertilityrate_country, X1980 != "")
fertilityrate_country <- subset(fertilityrate_country, X2018 != "")
summary(fertilityrate_country)

#6. Calculating the mean value for the fertility rate for both years used in the representation/analysis.
mean1960 <- mean(fertilityrate_country$X1980)
mean2018 <- mean(fertilityrate_country$X2018)
meanbirths <- c(mean1960, mean2018)

#7. Creating a boxplot of both years that will be used in the analysis (that includes a legend as well as a point for the mean value above).
boxplot(fertilityrate_country$X1980, fertilityrate_country$X2018,
        main="Boxplot of Fertility Rate in 190 Countries", 
        horizontal = FALSE, 
        ylab="Average Number of Children Given Birth To per Woman",
        names=c('Year 1980', 'Year 2018'),
        las=1)
points(meanbirths, col="blue", pch=17)
legend("topright", col= c("blue"), 
       c("mean value"), pch=17, inset=.04)

#8. Calculate the t-test to see whether the difference between the two distributions is statistically significantly significant.
test_value <- t.test(fertilityrate_country$X1980, fertilityrate_country$X2018, paired=TRUE)
test_value$p.value

#9. Create histograms to back up the data analysis that can be made from the creation of the boxplot. 
hist(fertilityrate_country$X1980,
     main="Histogram of Fertility Rate in 190 Countries (1980)",
     xlab="Fertility Rate (Births per Woman)",
     ylab="Number of Countries",
     col="yellow",
     border="black")

hist(fertilityrate_country$X2018,
     main="Histogram of Fertility Rate in 190 Countries (2018)",
     xlab="Fertility Rate (Births per Woman)",
     ylab="Number of Countries",
     col="yellow",
     border="black")

