####################
#Female Share of Employment in Senior and Middle Management (%)
managerial <- read.csv("managerial_full.csv")

#importing the file CountryCode which contains the Country Code, Region, and IncomeGroup
#and renaming the data as country
country <- read.csv("CountryCode.csv")

#selecting only the columns "Country.Code" and "Region" and renaming the data as "country"
country <- subset(country, select = c(Country.Code,Region,IncomeGroup))

#selecting only the rows where Region exists and renaming the data frame as "country"
country <- subset(country, Region != "")
#renaming the column names as "Country.Code" and "World.Region" and "Income.Group"
colnames(country) <- c("Country.Code","World.Region", "Income.Group")

#the dataset "country" is now cleaned

managerial_country <- merge(managerial, country, by="Country.Code")
summary(managerial_country)


#####Comparing the year 2000 and the year 2019
managerial_data<-subset(managerial_country, select=c("Country.Code", "Country.Name", 
                                                     "World.Region", "Income.Group", "X2000", "X2019"))
summary(managerial_data)
217-183
217-163


boxplot(managerial_data$X2000, managerial_data$X2019,
        main="Percent of Women in Managerial Role", 
        horizontal = FALSE, 
        ylab="Percentage",
        names=c('Year 2000 (34 countries)', 'Year 2019 (54 countries)'),
        las=1)

managerial_data <- na.omit(managerial_data)
summary(managerial_data)

boxplot(managerial_data$X2000, managerial_data$X2019,
        main="Percent of Women in Managerial Role in 29 countries", 
        horizontal = FALSE, 
        ylab="Percentage",
        names=c('Year 2000', 'Year 2019'),
        las=1)

test_value <- t.test(managerial_data$X2000, managerial_data$X2019, paired=TRUE)
test_value$p.value


#######Choosing Lithuania and Dominican Republic
managerial_max <- subset(managerial_country, 
                         Country.Name == "Lithuania" | Country.Name == "Dominican Republic")
managerial_max <- managerial_max[,5:65]
rownames(managerial_max) <- c("Dominican Republic", "Lithuania")

max_dataset <- as.data.frame(t(managerial_max))

Year <- c(1960:2020)
max_dataset <- cbind(max_dataset, Year)
max_dataset <- na.omit(max_dataset)

plot(max_dataset$Year, max_dataset$"Dominican Republic",
     xlab="Year", ylab="Percent of Women in Managerial Position",
     main="Percent in Managerial Position from 2000 to 2019",
     ylim=c(20,60), xlim=c(2000,2019), xaxt="n", type="o", col="green", las=1)
axis(side=1, at = c(2000:2019), labels = c(2000:2019), tck=.03)
lines(max_dataset$Year, max_dataset$Lithuania, type="o", col="blue")
legend("bottomright", col= c("green", "blue"), c("Dominican Republic", "Lithuania"), 
       pch = 1, inset = .04)

#######Choosing the United States
managerial_usa <- subset(managerial_country, Country.Name == "United States")
managerial_usa <- managerial_usa[,5:65]
rownames(managerial_usa) <- c("United States")

usa_dataset <- as.data.frame(t(managerial_usa))

Year <- c(1960:2020)
usa_dataset <- cbind(usa_dataset, Year)
usa_dataset <- na.omit(usa_dataset)

plot(usa_dataset$Year, usa_dataset$"United States",
     xlab="Year", ylab="Percent of Women in Managerial Position",
     main="Percent in Managerial Position in the United States from 2000 to 2019",
     ylim=c(34,42), xlim=c(2000,2019), xaxt="n", type="o", las=1)
axis(side=1, at = c(2000:2019), labels = c(2000:2019), tck=.03)

usa_dataset$Men <- 100-usa_dataset$"United States"
colnames(usa_dataset) <- c("Women", "Year", "Men")

plot(usa_dataset$Year, usa_dataset$Women,
     xlab="Year", ylab="Percent of Women in Managerial Position",
     main="Percent in Managerial Position in the United States from 2000 to 2019",
     ylim=c(0,80), xlim=c(2000,2019), xaxt="n", type="o", col="red", las=1)
axis(side=1, at = c(2000:2019), labels = c(2000:2019), tck=.03)
lines(usa_dataset$Year, usa_dataset$Men, type="o", col="blue")
legend("bottomright", col= c("red", "blue"), c("Female", "Male"), pch = 1, inset = .05)

##########Lithuania, Dominican Republic, USA
plot(max_dataset$Year, max_dataset$"Dominican Republic",
     xlab="Year", ylab="Percent of Women in Managerial Position",
     main="Percent in Managerial Position from 2000 to 2019",
     ylim=c(10,60), xlim=c(2000,2019), xaxt="n", type="o", col="green", las=1)
axis(side=1, at = c(2000:2019), labels = c(2000:2019), tck=.03)
lines(max_dataset$Year, max_dataset$Lithuania, type="o", col="yellow")
lines(usa_dataset$Year, usa_dataset$Women, type="o")
legend("bottomright", col= c("green", "yellow", "black"), 
       c("Dominican Republic", "Lithuania", "United States"), pch = 1, inset = .04)


#######Comparing Income.Group of all countries in 2019
managerial_2019 <- subset(managerial_country, 
                          select=c("Country.Name", 
                                   "X2019",
                                   "World.Region",
                                   "Income.Group"))
managerial_2019 <- as.data.frame(na.omit(managerial_2019))
summary(managerial_2019)

#finding the mean for each of the income group
mean_income <- tapply(managerial_2019$X2019, managerial_2019$Income.Group, FUN=mean)

barplot(mean_income, main="Mean value for percent of women in managerial positions",
        sub="(2019 dataset for 54 countries)",
        xlab="Income Group as defined by the World Bank", axis.lty=1,
        ylab="Mean percent of women in managerial roles",
        ylim=c(0,40), col="cyan", cex.names = 0.8, axes=TRUE)


boxplot(managerial_2019$X2019~managerial_2019$Income.Group,
        main="Percent of Women in Managerial Position based on Income Group in 2019 (n=54)",
        xlab="Income Group as defined by the World Bank",
        ylab="Percent of Women in Managerial Roles",las=1)

summary(aov(managerial_2019$X2019~managerial_2019$Income.Group))



#####################

