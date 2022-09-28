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

###################################
#Exam Practice Begins
###################################

#1. Write the line(s) of code that you need to run 
#to produce the following boxplot. 
#(See paper)
boxplot(uni$SAT.Math.25th.percentile.score~uni$ranking)$stats

#2. What does the boxplot tell you about 
#the students who are admitted to the top 50 schools vs.
#the students who are admitted to the non-ranked schools?
#


#3. Run the following code. Write down what value(s) the code return. 
subset(subset(uni, ranking == "Not ranked"), 
       SAT.Math.75th.percentile.score > 770)[,2]


#4. What information does the code in #3 give you? 
#How does the information relate to the boxplot below?
#(See paper)


#5. Write the codes to create this exact representation 
#along with the colors, labels, and legend.
#(See paper)

boxplot(uni$SAT.Math.75th.percentile.score~uni$SAT.Critical.Reading.75th.percentile.score,
        main="University Acceptance Rate per Type of Institution",
        sub = "777 universities in 1995",
        xlab="Type of Institution",
        ylab="Acceptance Rate",las=1, col="pink")
points(accrate_type, col="blue", pch=17)
legend("bottomright", col= c("blue"), 
       c("mean value"), pch=17, inset=.04)

plot(uni$SAT.Math.75th.percentile.score~uni$SAT.Critical.Reading.75th.percentile.score,
main="Relationship between the SAT Math and Critical Reading Scores for students who are in the 75th percentile",
sub = "The dataset is for 1534 universities in 2013",
xlab="SAT Math Score",
ylab="SAT Critical Reading Score",las=1, col="blue")
points(uni, col="blue", pch=17)
legend("topleft", col= c("red", "blue"), 
       c("Not Ranked", "Top 50"), pch=1, inset=.04)


plot(college$Acceptance.Rate, college$Gr, col=color_type)

boxplot(college$Acceptance.Rate~college$Type,
        main="University Acceptance Rate per Type of Institution",
        sub = "777 universities in 1995",
        xlab="Type of Institution",
        ylab="Acceptance Rate",las=1, col="pink")
points(accrate_type, col="blue", pch=17)
legend("bottomright", col= c("blue"), 
       c("mean value"), pch=17, inset=.04)


#6. How effective is the plot in #5 in providing insight about
#the students who are admitted to the top 50 schools vs.
#the students who are admitted to the non-ranked schools?
#That is, what are its limitations?
#What information might be missing from the representation?
#Based on this representation, can we conclude that a student 
#who scores high on the Math SAT is likely 
#to score high on the Critical Reading SAT also?


#7. Run the following codes to create eight parallel boxplots
#with their respective colors,
#and then analyze and evaluate what you see. 
#You do NOT need to look for outside sources. 
#The WMD paraphrase (not even direct quote) would suffice.
ranked_uni <- subset(uni, ranking == "Top 50")
not_ranked <- subset(uni, ranking == "Not ranked")
boxplot(ranked_uni$Percent.of.total.enrollment.that.are.Asian,
        not_ranked$Percent.of.total.enrollment.that.are.Asian,
        ranked_uni$Percent.of.total.enrollment.that.are.White,
        not_ranked$Percent.of.total.enrollment.that.are.White,
        ranked_uni$Percent.of.total.enrollment.that.are.Black.or.African.American,
        not_ranked$Percent.of.total.enrollment.that.are.Black.or.African.American,
        ranked_uni$Percent.of.total.enrollment.that.are.Hispanic.Latino,
        not_ranked$Percent.of.total.enrollment.that.are.Hispanic.Latino,
        col=c("green","orange"), boxlty=c(1,2)) $ stats
legend("topleft", col=c("green","orange"),
       c("Top 50", "Not ranked"), pch = 15, inset = .04)


###################################
#End of Exam Practice
###################################