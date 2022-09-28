#importing the file CountryCode which contains the Country Code and Region 
#and renaming the data as country
country <- read.csv("CountryCode.csv")

#selecting only the columns "X...Country.Code" and "Region" and renaming the data as "country"
country <- subset(country, select = c(X...Country.Code,Region))

#selecting only the rows where Region exists and renaming the data frame as "country"
country <- subset(country, Region != "")
#renaming the column names as "Country.Code" and "Region"
colnames(country) <- c("Country.Code","Region")

############################

#importing and reading the files import and inflation
import <- read.csv("import2019.csv")
inflation <- read.csv("inflation2019.csv")
#merging the import and inflation dataset and naming it "merge1"
merge1 <- merge(import, inflation, by="X...Country.Name")
merge1 <- subset(merge1, select=c(X...Country.Name,Country.Code.x,Import2019,Inflation2019))
colnames(merge1) <- c("Country.Name","Country.Code","Import2019","Inflation2019")
  
#merging the "country" and "merge1" data frames and naming it "paper"
paper <- merge(country, merge1, by="Country.Code")
#dropping all the rows with missing values and renaming the data frame as "paper"
paper <- na.omit(paper)

write.csv(paper, file="CorrelationPaperData.csv")
