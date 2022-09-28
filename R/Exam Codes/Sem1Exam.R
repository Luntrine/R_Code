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