







demographicData = read.csv("C:/Users/chintana/OneDrive/Masters/Projects/Dataset/original/national-health-and-nutrition-examination-survey/demographic.csv")  
colnames(demographicData)
str(demographicData)
sapply(demographicData, class)

# Changing Gender to Factor and revaluing Levels from 1,2 to Male, Female respectively

names(demographicData)[4]<-"Gender"
class(demographicData["Gender"])
demographicData$Gender <- as.factor(demographicData$Gender)
levels(demographicData$Gender)
levels(demographicData$Gender)[levels(demographicData$Gender)=="1"] <- "Male"
levels(demographicData$Gender)[levels(demographicData$Gender)=="2"] <- "Female"
levels(demographicData$Gender)

# Prepping and Cleaning AgeinYears and AgeinMonths
names(demographicData)[5]<-"AgeYears"
names(demographicData)[6]<-"AgeMonths"
names(demographicData)[10]<-"AgeMonthsU19"
demographicData[c(5:6,10)] <- lapply(demographicData[c(5:6,10)], as.numeric)


demographicData$AgeMonthsU19<-demographicData$AgeMonthsU19/12
demographicData$AgeMonthsU19<-round(demographicData$AgeMonthsU19,1)

demographicData$AgeMonths<-demographicData$AgeMonths/12
demographicData$AgeMonths<-round(demographicData$AgeMonths,1)

indicesMonths19<-which(!is.na(demographicData$AgeMonthsU19))
demographicData$AgeYears[indicesMonths19]<-demographicData$AgeMonthsU19[indicesMonths19]

indicesYears<-which(is.na(demographicData$AgeYears))
demographicData$AgeYears[indicesYears]<-demographicData$AgeMonths[indicesYears]


length(unique(demographicData$SEQN))


# Prepping Ethnic variables
names(demographicData)[7]<-"Race"
names(demographicData)[8]<-"RaceNHA"
demographicData$Race <- as.factor(demographicData$Race)
demographicData$RaceNHA <- as.factor(demographicData$RaceNHA)
levels(demographicData$Race)
levels(demographicData$RaceNHA)
levels(demographicData$Race) <- c("MexAmerican","OtherHisp","NHWhite","NHBlack","Other")
levels(demographicData$RaceNHA) <- c("NHA-MexAmerican","NHA-OtherHisp","NHA-NHWhite","NHA-NHBlack","NHA-NHAsian","Other")
levels(demographicData$Race)
levels(demographicData$RaceNHA)

#Prepping Birth and Citizen Columns
names(demographicData)[13]<-"BirthCountry"
names(demographicData)[14]<-"Citizenship"
demographicData$BirthCountry <- as.factor(demographicData$BirthCountry)
demographicData$Citizenship <- as.factor(demographicData$Citizenship)
levels(demographicData$BirthCountry) <- c("USA","NON-USA","Refused","DontKnow")
levels(demographicData$Citizenship) <- c("USCitizen","NonUSCitizen","Refused","DontKnow")
levels(demographicData$BirthCountry)
levels(demographicData$Citizenship)


names(demographicData)[15]<-"USStayLength"
demographicData$USStayLength <- as.factor(demographicData$USStayLength)
levels(demographicData$USStayLength) <- c("<1","1-5","5-10","10-15","15-20","20-30","30-40","40-50",">50","Refused","DontKnow")
levels(demographicData$USStayLength)

#Prepping Education Status Columns

names(demographicData)[16]<-"EduLevelChild"
names(demographicData)[17]<-"EduLevelAdult"
demographicData$EduLevelChild <- as.factor(demographicData$EduLevelChild)
demographicData$EduLevelAdult <- as.factor(demographicData$EduLevelAdult)
levels(demographicData$EduLevelChild)
levels(demographicData$EduLevelAdult)
levels(demographicData$EduLevelChild) <- c("NoEdu/KG","1Grade","2Grade","3Grade","4Grade","5Grade","6Grade","7Grade","8Grade","9Grade","10Grade","11Grade","12Grade","HighSchool","GED",">HighSchool",">5Grade",">9Grade","DontKnow")
#levels(demographicData$EduLevelAdult) <- c(">9Grade","9-11Grade","HighSchool/GED","College/AA","College/Above","Refused","DontKnow")
levels(demographicData$EduLevelAdult) <- c(">9Grade","9-11Grade","HighSchool/GED","College/AA","College/Above","Refused","DontKnow","NoEdu/KG","1Grade","2Grade","3Grade","4Grade","5Grade","6Grade","7Grade","8Grade","9Grade","10Grade","11Grade","12Grade","HighSchool","GED",">HighSchool",">5Grade",">9Grade","Elementary")
#levels(demographicData$EduLevelAdult) <- c("Elementary","HighSchool/GED","HighSchool/GED","College/AA","College/AA","Refused","DontKnow","NoEdu/KG","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","Elementary","HighSchool/GED","Elementary")




indicesNAEdu<-which(is.na(demographicData$EduLevelAdult))
demographicData$EduLevelAdult[indicesNAEdu]<-demographicData$EduLevelChild[indicesNAEdu]



#Prepping Marital Status/Pregnancy Columns


names(demographicData)[18]<-"MaritalStatus"
names(demographicData)[19]<-"PregnancyStatus"
demographicData$MaritalStatus <- as.factor(demographicData$MaritalStatus)
demographicData$PregnancyStatus <- as.factor(demographicData$PregnancyStatus)
levels(demographicData$MaritalStatus)
levels(demographicData$PregnancyStatus)
levels(demographicData$MaritalStatus) <- c("Married","Widowed","Divorced","Separated","NeverMarried","LivingIn","Refused","DontKnow")
levels(demographicData$PregnancyStatus) <- c("Pregnant","NotPregnant","CannotTell")
indicesMale<-which(demographicData$Gender=="Male")
demographicData$PregnancyStatus[indicesMale]==demographicData$Gender[indicesNAEdu]

#Prepping Family related variables

names(demographicData)[30]<-"PeopleHH"
names(demographicData)[31]<-"PeopleFamily"
names(demographicData)[45]<-"HHIncome"
names(demographicData)[46]<-"FamilyIncome"
names(demographicData)[47]<-"PovertyRatio"
demographicData$PeopleHH <- as.integer(demographicData$PeopleHH)
demographicData$PeopleFamily <- as.integer(demographicData$PeopleFamily)
demographicData$HHIncome <- as.factor(demographicData$HHIncome)
demographicData$FamilyIncome <- as.factor(demographicData$FamilyIncome)
demographicData$PovertyRatio <- as.numeric(demographicData$PovertyRatio)

levels(demographicData$PeopleHH)
levels(demographicData$PeopleFamily)
levels(demographicData$HHIncome)
levels(demographicData$FamilyIncome)

levels(demographicData$HHIncome) <- c("<5k","5-10k","10-15k","15-20k","20-25k","25-35k","35-45k","45-55k","55-65k","65-75k",">20k","<20k","75-99k",">100k","Refused","DontKnow")
levels(demographicData$FamilyIncome) <- c("<5k","5-10k","10-15k","15-20k","20-25k","25-35k","35-45k","45-55k","55-65k","65-75k",">20k","<20k","75-99k",">100k","Refused","DontKnow")


# Removing columns not considered for analysis

demographicData<-subset(demographicData, select=-c(2:3,9,11:12,20:29,32:44))
colnames(demographicData)

demographicData<-subset(demographicData, select=-c(4,7,11))
colnames(demographicData)



#######



###




write.csv(demographicData,"demographic_new.csv")


plot(demographicData$PovertyRatio,demographicData$FamilyIncome)

tbl1=table(demographicData$FamilyIncome,demographicData$MaritalStatus)
chisq.test(tbl1) 

cor.test(demographicData$FamilyIncome,demographicData$MaritalStatus, method='kendall')

table(demographicData$FamilyIncome,demographicData$EduLevelAdult)

correlationResult<-cor(demographicData)
print(demographicData)

lm(demographicData$MaritalStatus~demographicData$FamilyIncome+ demographicData$Gender)


###

colnames(demographicData)
summary(demographicData)
levels(demographicData$FamilyIncome)


