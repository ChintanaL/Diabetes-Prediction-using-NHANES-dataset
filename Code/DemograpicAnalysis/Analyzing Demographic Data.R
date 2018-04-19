#Analyzing Demoragpic Data


DemographicAnalysis<-merge(cleanDemographicData, FinalLabels, by='SEQN', all.y=TRUE)
length(unique(DemographicAnalysis$SEQN))

#Replace all NA values as zero in fina_idsDia

NAInd<-which(is.na(DemographicAnalysis$final_IsDia))
DemographicAnalysis$final_IsDia[NAInd]<-0
DemographicAnalysis<-DemographicAnalysis[,-18]

##
sapply(DemographicAnalysis,class)
DemographicAnalysis$final_IsDia<-as.factor(DemographicAnalysis$final_IsDia)
levels(DemographicAnalysis$final_IsDia)
levels(DemographicAnalysis$final_IsDia)<-c("No","Yes")
boxplot(DemographicAnalysis$PovertyRatio~DemographicAnalysis$final_IsDia,data=DemographicAnalysis, main="Diabetes vs Poverty Ratio")
##

tbl1<-table(DemographicAnalysis$final_IsDia, DemographicAnalysis$HHIncome)
chisq.test(tbl1) 


tbl2<-table(DemographicAnalysis$final_IsDia, DemographicAnalysis$PovertyRatio)
chisq.test(tbl2) 
## Refactoring 

#levels(DemographicAnalysis$EduLevelAdult) <- c(">9Grade","9-11Grade","HighSchool/GED","College/AA","College/Above","Refused","DontKnow")
#levels(demographicData$EduLevelAdult) <- c(">9Grade","9-11Grade","HighSchool/GED","College/AA","College/Above","Refused","DontKnow","NoEdu/KG","1Grade","2Grade","3Grade","4Grade","5Grade","6Grade","7Grade","8Grade","9Grade","10Grade","11Grade","12Grade","HighSchool","GED",">HighSchool",">5Grade",">9Grade","Elementary")
levels(DemographicAnalysis$EduLevelAdult) <- c("Elementary","HighSchool/GED","HighSchool/GED","College/AA","College/AA","Refused","DontKnow","NoEdu/KG","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","Elementary","HighSchool/GED","Elementary")


## Data Parition

library(caret)
trainIndex = createDataPartition(DemographicAnalysis$final_IsDia, p=0.7, list=FALSE)

train = DemographicAnalysis[trainIndex,]
test = DemographicAnalysis[-trainIndex,]
nrow(train)
nrow(test)
colnames(train)

##Naive Bayes, Logistic and SVM

#model <- glm(final_IsDia~Gender+AgeYears+FamilyIncome+PovertyRatio+EduLevelAdult,family=binomial(link='logit'),data=train)
model <- NaiveBayes(final_IsDia~Gender+AgeYears+FamilyIncome+PovertyRatio+EduLevelAdult, data=train)
model <- svm(final_IsDia~Gender+AgeYears+FamilyIncome+PovertyRatio+EduLevelAdult, data=train)
summary(model)

## make predictions
x_test <- test[,c('Gender','AgeYears','FamilyIncome','PovertyRatio','EduLevelAdult','RaceNHA','FamilyIncome')]
y_test <- test[,'final_IsDia']
predProb <- predict(model, x_test, type="response")

predictions$class
# summarize results
confusionMatrix(predictions$class, y_test)



### SVM
colnames(test)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(final_IsDia~Gender+AgeYears+FamilyIncome+PovertyRatio+EduLevelAdult+RaceNHA+FamilyIncome, data = train, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

x_test[,'test_pred_svm'] <- predict(svm_Linear, newdata = x_test)
test$final_IsDia
confusionMatrix(x_test[,'test_pred_svm'], test[,'final_IsDia'] )

###
