#Running PCA on finaldataset with cleant diet, data and demo combined with final labels

#
colnames(finalexamDemoDiet)
str(finalexamDemoDiet)


finalexamDemoDiet<- read.csv("C:/Users/chintana/OneDrive/Masters/Projects/Dataset/Cleant/finalexamDemoDiet.csv", stringsAsFactors = F)
#View(final)

nums <- unlist(lapply(finalexamDemoDiet, is.numeric))
nums<-which(nums)
finalexamDemoDietNUM<-finalexamDemoDiet[,nums]
finalexamDemoDietNUM<-finalexamDemoDietNUM[,-1]
colnames(finalexamDemoDietNUM)

#max = apply(finalexamDemoDietNUM , 2 , max)
#min = apply(finalexamDemoDietNUM, 2 , min)
#scaled = as.data.frame(scale(finalexamDemoDietNUM, center = min, scale = max - min))


#library(caret)
set.seed(123)
trainIndex = createDataPartition(finalexamDemoDietNUM$is_diabetic, p=0.95, list=FALSE)
nrow(finalexamDemoDietNUM)

train = finalexamDemoDietNUM[trainIndex,]
test = finalexamDemoDietNUM[-trainIndex,]
nrow(train)
nrow(test)
colnames(train)

trainAll = finalexamDemoDiet[trainIndex,]
testAll = finalexamDemoDiet[-trainIndex,]
nrow(trainAll)
nrow(testAll)
colnames(trainAll)
str(trainAll)
numsFactor <- unlist(lapply(finalexamDemoDiet, is.character))
which(numsFactor)

## Naive Bayes

library('e1071')
naive_bayes_model <- naiveBayes(as.factor(is_diabetic)~., data = train)
nb_predict <- predict(naive_bayes_model, newdata = test[-c(3)],type = "class")
#View(nb_predict)
conf_matrix_nb <- table(nb_predict,test$is_diabetic)
confusionMatrix(conf_matrix_nb)


## Naive Bayes with All

library('e1071')
naive_bayes_modelAll <- naiveBayes(as.factor(is_diabetic)~., data = trainAll)
nb_predictAll <- predict(naive_bayes_modelAll, newdata = testAll[-c(3)],type = "class")
#View(nb_predict)
conf_matrix_nbAll <- table(nb_predictAll,testAll$is_diabetic)
confusionMatrix(conf_matrix_nbAll)
  
library(dplyr)
pca_trainset = train %>% select( -c(is_diabetic) )
pca_testset = test[-c(3)]
nrow(pca_trainset)
pca = prcomp( pca_trainset, scale = T )
names(pca)
# variance
pr_var = ( pca$sdev )^2 

# % of variance
prop_varex = pr_var / sum( pr_var )

# Plot
plot( prop_varex, xlab = "Principal Component", 
      ylab = "Proportion of Variance Explained", type = "b" )


plot( cumsum( prop_varex ), xlab = "Principal Component", 
      ylab = "Cumulative Proportion of Variance Explained", type = "b" )

##### MAke new data now with PCA dimension data and previously omitted factor variables
trainPCA = data.frame( is_diabetic = train$is_diabetic,Gender=trainAll$Gender,USStayLength=trainAll$USStayLength,PregnancyStatus=trainAll$PregnancyStatus,FamilyIncome=trainAll$FamilyIncome, pca$x )
trainPCA <- trainPCA[,1:19]

# Train NB again now with PCA tranformed train data

naive_bayes_model_PCA <- naiveBayes(as.factor(is_diabetic)~., data = trainPCA)

#Change test data into PCA

testPCA <- predict(pca, newdata = test)
testPCA <- data.frame(testPCA,testAll$Gender,USStayLength=testAll$USStayLength,PregnancyStatus=testAll$PregnancyStatus,FamilyIncome=testAll$FamilyIncome)

#select the first 17 components
testPCA <- testPCA[,-c(15:25)]

#make prediction on test data

nb_predictPCA <- predict(naive_bayes_model_PCA, newdata = testPCA,type = "class")
#View(nb_predictPCA)
conf_matrix_nb_PCA <- table(nb_predictPCA,test$is_diabetic)
confusionMatrix(conf_matrix_nb_PCA)


## try on SVM before and after

svm_model <- svm(is_diabetic~.,data = trainPCA)
svm_predict <- predict(svm_model,newdata = testPCA)
#conf_matrix_svm <- table(svm_predict,test$is_diabetic)
#confusionMatrix(conf_matrix_svm)
library(ROCR)
pr_svm <- prediction(svm_predict,testAll$is_diabetic)
prf_svm <- performance(pr_svm,measure = "tpr",x.measure = "fpr")
plot(prf_svm)
default.pred_svm <- ifelse(svm_predict > 0.2, 1, 0)
default.pred_svm2 <- ifelse(svm_predict > 0.3, 1, 0)
default.pred_svm3 <- ifelse(svm_predict > 0.4, 1, 0)
conf_matrix_svm <- table(default.pred_svm3,test$is_diabetic)
confusionMatrix(conf_matrix_svm)








