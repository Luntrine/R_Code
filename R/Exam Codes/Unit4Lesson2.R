college <- read.csv("college1995.csv")

college$Private[college$Private == "Yes"] <- c("Private")
college$Private[college$Private == "No"] <- c("Public")
colnames(college)[2] <- c("Type")

college$Acceptance.Rate <- college$Accept/college$Apps

accrate_type <- tapply (college$Acceptance.Rate, college$Type, FUN=mean)
accrate_type

boxplot(college$Acceptance.Rate~college$Type,
        main="University Acceptance Rate per Type of Institution",
        sub = "777 universities in 1995",
        xlab="Type of Institution",
        ylab="Acceptance Rate",las=1, col="pink")
points(accrate_type, col="blue", pch=17)
legend("bottomright", col= c("blue"), 
       c("mean value"), pch=17, inset=.04)

plot(college$Acceptance.Rate, college$Gr)

college$Type <- as.factor(college$Type)
color_type = c("red", "blue")[college$Type]
plot(college$Acceptance.Rate, college$Gr, col=color_type)

plot(college$Expend, college$S.F.Ratio, col=color_type)
legend("topright", col= c("blue", "red"), 
       c("Public", "Private"), pch=1, inset=.04)

############### EXTRA BEGINS
model_expend_sfratio <- lm(college$S.F.Ratio~I(1/college$Expend))
summary(model_expend_sfratio)

predicted_values <- model_expend_sfratio$coefficients
x<-seq(0,60000,1000)
predicted_inv <- predicted_values[2]*1/x + predicted_values[1]
points(x,predicted_inv, type="l", col="green", lwd=2)

############### EXTRA ENDS

boxplot((college$Expend/1000)~college$Type,
        main="University Spending",
        sub = "777 universities in 1995",
        xlab="Type of Institution",
        ylab="Instructional Spending per Student (in $1000)",las=1, col="royalblue")

boxplot(college$S.F.Ratio~college$Type,
        main="University Student to Faculty Ratio",
        sub = "777 universities in 1995",
        xlab="Type of Institution",
        ylab="Student Faculty Ratio",las=1, col="tomato")

plot(college$Apps, college$Accept, col=color_type)
model_app_accept <- lm(college$Accept~college$Apps)
abline(model_app_accept)
summary(model_app_accept)

plot(college$Top10perc, college$Expend, col=color_type)
abline(lm(college$Expend~college$Top10perc))
summary(lm(college$Expend~college$Top10perc))

plot(college$Top10perc, college$S.F.Ratio, col=color_type)
abline(lm(college$S.F.Ratio~college$Top10perc))
summary(lm(college$S.F.Ratio~college$Top10perc))

