#install.packages("gplots")
library(gplots)
#install.packages("ROCR")
library(ROCR)
#install.packages("caret")
#install.packages("lattice")
#install.packages("ggplot2")
library(lattice)
library(ggplot2)
library(caret)
library(dplyr)

final<- read.csv("C:/Users/chintana/OneDrive/Masters/Projects/Dataset/Cleant/finalexamDemoDiet.csv", stringsAsFactors = F)
#View(final)
str(final)
final2 <- final %>% mutate_if(is.character, as.factor)
final2<-final2[,-c(1,2)]
str(final2)
#install.packages("FactoMineR")
ncol(final)
library(FactoMineR)
#pca3 = PCA(USArrests, graph = FALSE)
sample_size <- floor(0.80*nrow(final))
set.seed(123)
#+train_ind <- sample(seq_len(nrow(pick_labs)), size = sample_size)
#+train <- pick_labs[train_ind,]
#+test <- pick_labs[-train_ind,]
train_ind <- sample(seq_len(nrow(final2)), size = sample_size)
train <- final2[train_ind,]
test <- final2[-train_ind,]


write.csv(train,"train.csv")
write.csv(test,"test.csv")
#model1 <- glm(is_diabetic~.,family = binomial(link = 'logit'), data = train[-1])

#final_predict <- predict(model1,test,type = "response")


#pr <- prediction(final_predict,test$is_diabetic)
#prf <- performance(pr,measure = "tpr",final.measure = "fpr")
#plot(prf)
#defaulted.pred <- ifelse(final_predict > 0.6,1,0)
#conf_matrix <- table(defaulted.pred,test$is_diabetic)

#confusionMatrix(conf_matrix)

# logistic Regression
train <- read.csv('train_PCA.csv')
train <- train[-1]
test <- read.csv('test_PCA.csv')
test <- test[-1]
model1 <- glm(is_diabetic~.,family = binomial(link = 'logit'), data = train)
x_predict <- predict(model1,test,type = "response")
library(ROCR)
pr <- ROCR::prediction(x_predict,test$is_diabetic)
prf <- performance(pr,measure = "tpr",final.measure = "fpr")
plot(prf, main = " lr_ROC Curve", ylab = " Sensitivity" , xlab = "1-Specificity")
auc <- performance(pr, "auc")
auc<- unlist(slot(auc, "y.values"))
auc <- round(auc,4)
legend(.2,.2,auc, title = "AUC")
defaulted.pred <- ifelse(x_predict > 0.5,1,0)
conf_matrix <- table(defaulted.pred,test$is_diabetic)
library('caret')
confusionMatrix(conf_matrix)

# naive bayes model

library('e1071')
naive_bayes_model <- naiveBayes(as.factor(is_diabetic)~., data = train)
nb_predict <- predict(naive_bayes_model, newdata = test,type = "class")
#View(nb_predict)
conf_matrix_nb <- table(nb_predict,test$is_diabetic)
confusionMatrix(conf_matrix_nb)
plot(naive_bayes_model)

# SVM model
#ncol(x)
svm_model <- svm(is_diabetic~.,data = train)
svm_predict <- predict(svm_model,newdata = test)
#conf_matrix_svm <- table(svm_predict,test$is_diabetic)
#confusionMatrix(conf_matrix_svm)
library(ROCR)
pr_svm <- ROCR::prediction(svm_predict,test$is_diabetic)
prf_svm <- performance(pr_svm,measure = "tpr",x.measure = "fpr")
plot(prf_svm,main = " SVM_ROC Curve", ylab = " Sensitivity" , xlab = "1-Specificity")
abline(a=0,b=1)
auc <- performance(pr_svm, "auc")
auc<- unlist(slot(auc, "y.values"))
auc <- round(auc,4)
legend(.8,.2,auc, title = "AUC")
#default.pred_svm <- ifelse(svm_predict > 0.2, 1, 0)
#default.pred_svm2 <- ifelse(svm_predict > 0.3, 1, 0)
default.pred_svm3 <- ifelse(svm_predict > 0.4, 1, 0)
conf_matrix_svm <- table(default.pred_svm3,test$is_diabetic)
confusionMatrix(conf_matrix_svm)


# KNN model

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
str(train)
norm_train <- as.data.frame(lapply(train[-c(1,3,4,5)], normalize))
norm_test <- as.data.frame(lapply(test[-c(1,3,4,5)], normalize))
#View(norm_test)
dim(norm_train)
dim(norm_test)
model_knn <- train(norm_train[,-3],as.factor(norm_train[,3]),method = 'knn')
predict_knn <- predict(object = model_knn, norm_test[,-3])
conf_matrix_knn <- table(predict_knn,norm_test[,3])
confusionMatrix(conf_matrix_knn)

# random forest model

install.packages('randomForest')
library('randomForest')
is.na(final)
str(final)
summary(final2)
str(final2)
library(dplyr)
#final2 <- final %>% mutate_if(is.character, as.factor)
#str(final2)
rf <- randomForest(as.factor(is_diabetic)~., ntree=100, data = train, importance=TRUE)
rf
varImpPlot(rf)
predict_rf <- predict(rf,test)
View(predict_rf)
conf_martix_rf <- table(predict_rf, test$is_diabetic)
confusionMatrix(conf_martix_rf)

# Neural Networks

install.packages('nnet')
library('nnet')
library(ROCR)
library(gplots)
lm.fit <- glm(is_diabetic~., data = train)
pr.lm <- predict(lm.fit, test[-9])
MSE.lm <- sum((pr.lm - test$is_diabetic)^2)/nrow(test)
apply(train, 2, function(x) sum(is.na(x)))
# add this line to remove non-numeric columns.
final3 <- final2[, sapply(final2, is.numeric)]
maxs <- apply(final3, 2, max)
mins <- apply(final3, 2, min)

scaled <- as.data.frame(scale(final3, center = mins, scale = maxs - mins))
index <- sample(1:nrow(final3),round(0.80*nrow(final3)))
train_new <- scaled[index,]
test_new <- scaled[-index,]

###################**********
install.packages('neuralnet')
library(neuralnet)
#n <- names(train_new)
#f <- as.formula(paste("is_diabetic~", paste(n[!n %in% "is_diabetic"], collapse = "+")))
#nnet_model <- neuralnet(f, data = train_new, hidden = c(5,3),linear.output = FALSE, stepmax = 1e6)
#plot(nnet_model)
#pr.nn <- compute(nnet_model, test_new[,1:9])
#pr.nn_new <- pr.nn$net.result*(max(pick_labs1$is_diabetic)- min(pick_labs1$is_diabetic))+ min(pick_labs1$is_diabetic)
#test.r <- (test_new$is_diabetic)*(max(pick_labs1$is_diabetic) - min(pick_labs1$is_diabetic))+min(pick_labs1$is_diabetic)
#MSE.nn <- sum((test.r - pr.nn_new)^2)/nrow(test_new)
#print(paste(MSE.lm,MSE.nn))
### Updated code for Neural Net below- chin

library( neuralnet )
n = names(train_new)
f = as.formula( paste( "is_diabetic ~", paste( n[!n %in% c("is_diabetic","SEQN")], collapse = "+" ) ) )
#nn = neuralnet( f, train_new, hidden = 2, linear.output = FALSE, threshold = 0.4 ) #Accuracy : 0.8690779 
#nn = neuralnet( f, train_new, hidden = 3, linear.output = FALSE, threshold = 0.4 ) #Accuracy : 0.8634743
#nn = neuralnet( f, train_new, hidden = 2, linear.output = FALSE, threshold = 0.5 ) #Accuracy : 0.8700968
nn = neuralnet( f, train_new, hidden = 3, linear.output = FALSE, threshold = 0.5 ) # Accuracy : 0.8930209   


#summary(nn)
plot( nn, rep = "best" )
#test<-test[-c(1,40)]
colnames(test_new[,-3])
predictNN <- neuralnet::compute(nn, test_new[,-3])

# Confusion matrix
#library( caret )
tableNN<- table(round( predictNN$net.result ),test_new$is_diabetic )
confusionMatrix(tableNN)

# GBM 

install.packages('gbm')
library('gbm')
library(caret)
gbm_model = gbm(formula = is_diabetic~.,distribution = "bernoulli",data = train,n.trees = 2500,shrinkage = .01,n.minobsinnode = 20, cv.folds = 5)
best.iter <- gbm.perf(gbm_model, method = "cv")
best.iter
summary(gbm_model)
predict_gbm <- predict(object = gbm_model,newdata = test,n.trees = 1500,type = "response")
conf_matrix_gbm <- data.frame("Actual"= test$is_diabetic, "predicted" = predict_gbm)
library(ROCR)
h <- getElement(test,"is_diabetic")
g<- as.data.frame(t(test$is_diabetic))
#View(test)
pr_gbm <- prediction(predict_gbm,test[,7])
prf_gbm <- performance(pr_gbm,measure = "tpr",x.measure = "fpr")
plot(prf_gbm,main = " gbm_ROC Curve", ylab = " Sensitivity" , xlab = "1-Specificity")
auc <- performance(pr_gbm, "auc")
auc<- unlist(slot(auc, "y.values"))
auc <- round(auc,4)
legend(.8,.2,auc, title = "AUC")
default.pred_gbm <- ifelse(predict_gbm > 0.4,1,0)
#default.pred_gbm2 <- ifelse(predict_gbm > 0.2,1,0)
conf_matrix_gbm1 <- table(default.pred_gbm,test[,7])
confusionMatrix(conf_matrix_gbm1)

# CART 
install.packages("rpart")
install.packages("rpart.plot")
library('rpart')
library('rpart.plot')
library(caret)
set.seed(123)
#cart_tree <- rpart(is_diabetic~., data = train[-1],control = rpart.control(cp=0.0001))
#printcp(cart_tree)
#bestcp <- cart_tree$cptable[which.min(cart_tree$cptable[,"xerror"]),"CP"]
#tree.pruned <- prune(cart_tree, cp = bestcp)
#cart_predict <- predict(tree.pruned, data = test[-c(1,10)], type = "class")
trctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(1234)
dtree_fit <- train(as.factor(is_diabetic)~., data = train, method = "rpart", parms = list(split = "information"), trControl = trctrl, tuneLength = 10)
dtree_fit
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
cart_predict <- predict(dtree_fit, newdata = test[,-(7)])
conf_matrix_cart <- table(cart_predict, test$is_diabetic)
confusionMatrix(conf_matrix_cart)

# Decision Tree
install.packages('rattle')
install.packages('RColorBrewer')
library('rattle')
library('RColorBrewer')

dectree <- rpart(is_diabetic~., data = train, method = "class", parms = list(split = "information"))
summary(dectree)
fancyRpartPlot(dectree)
preddecTree <- predict(dectree, test, type = "class")
conf_matrix_dectree <- table(preddecTree,test$is_diabetic)
confusionMatrix(conf_matrix_dectree)
