rm(list=ls())
#importing IPEDS dataset
IPEDS <- read.csv("IPEDS_data.csv")
#importing US News Ranking dataset
ranking <- read.csv("ranking_data.csv")
#adding a column in the ranking dataset to add "Top 50"
ranking$ranking <- c("Top 50")
#changing the second column name into "ID.number" 
#so that we can merge it later with the IPEDS dataset by the college IPEDS ID number
colnames(ranking)[2] <- c("ID.number")

#merging the two dataset together by their IPEDS ID number, keeping all columns
uni <- merge(IPEDS, ranking, by="ID.number", all=TRUE)

#changing the NA values in "ranking" column to 0 
#and then changing the value 0 into a categorical value "Not ranked"
uni$ranking[is.na(uni$ranking)] = 0
uni$ranking[uni$ranking == 0] <- "Not ranked"

#checking to make sure that we have 61 "Top 50" universities 
#to match the US News ranking dataset
table(uni$ranking)

#######################################
#The data frame "uni" will be used for processing from this point on.
#######################################

#1. What information does this line give you? What does it mean?
table(uni$ranking, uni$Historically.Black.College.or.University)

#2. What information does this line give you? What does it mean?
prop.table(table(uni$ranking, uni$Historically.Black.College.or.University),1)*100

#3. Is it true that the tuition and fees for ranked universities 
#are higher than the tuition and fees for non-ranked universities? 
#Use the boxplot to explain your answer.
boxplot(uni$Tuition.and.fees..2013.14~uni$ranking)$stats

#4. Explain the difference between the following boxplot functions. 
#Do not try to run the codes in R. It won’t work. 
#The x, y, x1, x2 are only examples in this question.
#boxplot(x~y) vs.boxplot(x1, x2)

#5. What information does the following lines of code give you? 
#What conclusions can you draw from the representation?
boxplot(uni$Tuition.and.fees..2010.11, 
        uni$Tuition.and.fees..2011.12, 
        uni$Tuition.and.fees..2012.13, 
        uni$Tuition.and.fees..2013.14,
        main="Boxplot of Fertility Rate in 190 Countries", 
        horizontal = FALSE,
        xlab="Schoolyear",
        ylab="Tuition and Fees in USD",
        names=c('2010-11', '2011-12', '2012-13', '2013-14'),
        las=1)


#6. Label the following boxplot with a title, x-axis label, and y-axis label
#so that the representation provides sufficient information for understanding
boxplot(uni$Tuition.and.fees..2010.11, uni$Tuition.and.fees..2011.12, 
        uni$Tuition.and.fees..2012.13, uni$Tuition.and.fees..2013.14)

#7. What conclusions can you make from this data representation?
plot(uni$X2013, uni$Tuition.and.fees..2013.14)

#8. What conclusions can you make from this data representation?
highest_degree <- table(uni$ranking, uni$Highest.degree.offered)
barplot(highest_degree,
        main="Highest Degrees Offered in Top 50 vs. Not Ranked Universities",
        xlab="Highest Degrees Offered", col=c("darkblue","red"),
        ylab="Number of Universities",
        ylim=c(0,600),
        names.arg=c("Bachelor's Degree", "D-Professional", 
                    "D-Other", "D-Research",
                    "D-Professional&Research", "Master's"), cex.names=0.8)
legend("topleft", col=c("darkblue","red"), 
       c("Not ranked", "Top 50"), pch = 15, inset = .04)



#9. Run the linear regression and answer the following questions.
#a. Summarize the general trend that you see in the representation.
#b. Interpret the slope of the line of best fit.
#c. What questions might you have about the representation and dataset?
uni$ranking <- as.factor(uni$ranking)
color_type = c("red", "blue")[uni$ranking]
plot(uni$Tuition.and.fees..2013.14, 
     uni$Graduation.rate...Bachelor.degree.within.4.years..total,
     col=color_type)
model1 <- lm(uni$Graduation.rate...Bachelor.degree.within.4.years..total~
               uni$Tuition.and.fees..2013.14)
summary(model1)
abline(model1)


#10. What return value does the following code give? 
#Explain what the code does.
subset(subset(uni, ranking == "Top 50"), 
       Graduation.rate...Bachelor.degree.within.4.years..total <20)[,2]

#11. Run the linear regression and answer the following questions.
#a. Summarize the general trend that you see in the representation.
#b. How is this different from the scatterplot and linear regression in #9?
plot(uni$Tuition.and.fees..2013.14, 
     uni$Graduation.rate...Bachelor.degree.within.5.years..total,
     col=color_type)
model2 <- lm(uni$Graduation.rate...Bachelor.degree.within.5.years..total~
               uni$Tuition.and.fees..2013.14)
summary(model2)
abline(model2)

#12. Run the linear regression.
#a. Interpret the slope.
#b. What conclusions can you make from this data representation? 
#What are the limitations?
plot(uni$X2013,
     uni$SAT.Math.75th.percentile.score)
model3 <- lm(uni$SAT.Math.75th.percentile.score~uni$X2013)
abline(model3)
summary(model3)


#######################################
