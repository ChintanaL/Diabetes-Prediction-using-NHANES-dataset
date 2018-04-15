 install.packages("mi")
 library(mi)
 
sum(is.na(demographicData$FamilyIncome))
sum(demographicData$FamilyIncome=="Other")
levels(demographicData$FamilyIncome)
table(demographicData$FamilyIncome)
table(demographicData$HHIncome)
colnames(demographicData)
sapply(demographicData,class)
table(demographicData$AgeYears)
sum(is.na(demographicData$PovertyRatio))
sum(is.na(demographicData$AgeYears))

table(demographicData$EduLevelAdult)

#Marital Status and Pregnancy Status 
table(demographicData$MaritalStatus)
sum(is.na(demographicData$MaritalStatus))
table(demographicData$MaritalStatus)
sum(is.na(demographicData$PregnancyStatus))
sum(is.na(cleanDemographicData$PregnancyStatus))


#Income
sum(is.na(demographicData$HHIncome))
sum(is.na(demographicData$FamilyIncome))


sum(is.na(demographicData$PeopleHH))
summary(demographicData$PeopleHH)
table(demographicData$PregnancyStatus)



#

install.packages("Hmisc")
library(Hmisc)
install.packages("missForest")
library(missForest)
install.packages("mice")
library(mice)
data("iris")
summary(iris)
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

iris.mis$imputed_age <- with(iris.mis, impute(Sepal.Length, mean))
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                           Species, data = iris.mis, n.impute = 5)

impute_arg$imputed$Sepal.Length


md.pattern(demographicData)
tempData <- mice(demographicData,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

demographicDataTest<-demographicData
colnames(demographicDataTest)

impute_arg <- aregImpute(~ Gender + AgeYears + Race + RaceNHA +HHIncome+FamilyIncome+PovertyRatio, data = demographicDataTest, n.impute = 5)


demographicDataTest.impRF <- missForest(demographicDataTest)

demographicDataTest.impRF$OOBerror
cleanDemographicData<-demographicDataTest.impRF$ximp







