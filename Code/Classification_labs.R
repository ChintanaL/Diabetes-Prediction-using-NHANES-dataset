#setwd("C:/Users/HP USER/Desktop/Spring 2018/MATH 571/Project")
medication <- read.csv("medications.csv")
medication[medication== ""] <- NA
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
medication_new<- completeFun(medication,"RXDDRUG")
medication_new<- completeFun(medication_new,"RXDDRGID")
medication_new<- completeFun(medication_new,"RXDRSD1")
medication_new<- completeFun(medication_new,"RXQSEEN")
medication_new<- completeFun(medication_new,"RXDRSC1")
medication_new<- completeFun(medication_new,"RXDCOUNT")
medication_new <- subset(medication_new, select = -RXDRSC2)
View(medication_new)
medication_new <- subset(medication_new, select = -RXDRSC3)
medication_new <- subset(medication_new, select = -RXDRSD2)
medication_new <- subset(medication_new, select = -RXDRSD3)
medication_new["Is_Diabetic"] <- NA
medication_new$Is_Diabetic  <- ifelse(medication_new$RXDRSC1 == "E11", 1 , 0)
medication_new <- subset(medication_new, select = c(SEQN,Is_Diabetic))


#meds <- medication_new%>%
 # select(SEQN,Is_Diabetic) %>%
  #group_by(SEQN) 
library(plyr)
meds <- ddply(medication_new,.(SEQN), summarize, Is_Diabetic = paste(Is_Diabetic, collapse = ","))
library(splitstackshape)
meds_1 <- cSplit(meds, "Is_Diabetic", ",")

library(tidyr)
library(dplyr)

meds_2 <- mutate(meds_1, final_IsDia = ifelse(Is_Diabetic_01 | Is_Diabetic_02 |Is_Diabetic_03 |Is_Diabetic_04 |Is_Diabetic_05 |Is_Diabetic_06 |Is_Diabetic_07|Is_Diabetic_08 |Is_Diabetic_09 |Is_Diabetic_10 |Is_Diabetic_11 |Is_Diabetic_12 |Is_Diabetic_13 |Is_Diabetic_14 |Is_Diabetic_15 |Is_Diabetic_16 |Is_Diabetic_17 |Is_Diabetic_18 |Is_Diabetic_19 |Is_Diabetic_20 |Is_Diabetic_20 |Is_Diabetic_21 |Is_Diabetic_22 |Is_Diabetic_23  == 1, 1 , 0))

meds_2 <- subset(meds_2, select = c(SEQN,final_IsDia))

meds_2[is.na(meds_2)] <- 0

View(meds_2)

write.csv(meds_2, 'medication_new.csv')


library(Hmisc)
library(SASxport)
lookup.xport("PBCD_H.XPT")
labs1 <- read.xport("PBCD_H.XPT")
lookup.xport("TCHOL_H.XPT")
labs2 <- read.xport("TCHOL_H.XPT")
lookup.xport("CBC_H.XPT")
labs3 <- read.xport("CBC_H.XPT")
lookup.xport("GHB_H.XPT")
labs4 <- read.xport("GHB_H.XPT")
lookup.xport("INS_H.XPT")
labs5<- read.xport("INS_H.XPT")
lookup.xport("OGTT_H.XPT")
labs6<- read.xport("OGTT_H.XPT")
labs1_new <- subset(labs1, select = c (SEQN,LBXBCD,LBXBPB,LBXBMN,LBXTHG,LBXBSE))
labs2_new <- subset(labs2, select = c(SEQN,LBXTC))
labs3_new <- subset(labs3, select = c(SEQN,LBDLYMNO,LBDMONO,LBDNENO,LBDEONO,LBDBANO,LBXRBCSI,LBXHGB,LBXPLTSI))
labs5_new <- subset(labs5, select = c(SEQN,LBXIN,PHAFSTHR))
labs6_new <- subset(labs6, select = c(SEQN,LBXGLT,GTXDRANK))
labs_new <- merge(labs1_new,labs2_new, by = "SEQN")
labs_new <- merge(labs_new,labs3_new, by = "SEQN")
labs_new <- merge(labs_new,labs4, by = "SEQN")
labs_new <- merge(labs_new,labs5_new, by = "SEQN")
labs_new <- merge(labs_new,labs6_new, by = "SEQN")
for(i in 1:ncol(labs_new)){
  labs_new[is.na(labs_new[,i]), i] <- mean(labs_new[,i], na.rm = TRUE)
}
labs_new["Is_Diabetic"] <- NA
labs_new$Is_Diabetic  <- ifelse(labs_new$LBXGH >= 6.0, 1 , 0)
write.csv(labs_new,'labs_new.csv')

setwd('C:/Users/HP USER/Desktop/Spring 2018/MATH 571/Project/DPAProject-master')
finals_E <- read.csv('finalLabels_18thApril_allE.csv')
labs <- read.csv('labs_new.csv') 
labs <- labs[,-1]
labs <- labs[,-21]
labs_final <- merge(finals_E,labs, by = "SEQN")
labs_final <- labs_final[-2]
names <- colnames(labs_final)
n_names <- names[-2]
all <- lapply(n_names,function(i) list(x = labs_final[,i],y= labs_final[,"is_diabetic"]))
corvar<-lapply(all, function(x) do.call(cor,x))
names(corvar) <- n_names
#corr_var <- corr_var[-c(2,4)]
corvar <- sort(unlist(corvar),decreasing = TRUE)
model <- glm(is_diabetic~.,family = binomial(link = 'logit'),data = labs_final)
summary(model)
pick_labs <- labs_final[c(1,4,5,8,9,14,17,18,19,20,2)]
sample_size <- floor(0.80*nrow(pick_labs))
set.seed(123)
train_ind <- sample(seq_len(nrow(pick_labs)), size = sample_size)
train <- pick_labs[train_ind,]
test <- pick_labs[-train_ind,]
model1 <- glm(is_diabetic~.,family = binomial(link = 'logit'), data = train[-1])
labs_predict <- predict(model1,test,type = "response")
library(ROCR)
pr <- prediction(labs_predict,test$is_diabetic)
prf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
defaulted.pred <- ifelse(labs_predict > 0.6,1,0)
conf_matrix <- table(defaulted.pred,test$is_diabetic)
confusionMatrix(conf_matrix)

pick_labs1 <- pick_labs[-7]
View(pick_labs1)
sample_size <- floor(0.80*nrow(pick_labs1))
set.seed(123)
train_ind <- sample(seq_len(nrow(pick_labs1)), size = sample_size)
train <- pick_labs1[train_ind,]
test <- pick_labs1[-train_ind,]

# logistic Regression

model1 <- glm(is_diabetic~.,family = binomial(link = 'logit'), data = train[-1])
labs_predict <- predict(model1,test,type = "response")
library(ROCR)
pr <- prediction(labs_predict,test$is_diabetic)
prf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
defaulted.pred <- ifelse(labs_predict > 0.5,1,0)
conf_matrix <- table(defaulted.pred,test$is_diabetic)
library('caret')
confusionMatrix(conf_matrix)

# naive bayes model

library('e1071')
naive_bayes_model <- naiveBayes(as.factor(is_diabetic)~., data = train[-1])
nb_predict <- predict(naive_bayes_model, newdata = test[-c(1,10)],type = "class")
View(nb_predict)
conf_matrix_nb <- table(nb_predict,test$is_diabetic)
confusionMatrix(conf_matrix_nb)

# SVM model

svm_model <- svm(is_diabetic~.,data = train[-1])
svm_predict <- predict(svm_model,newdata = test[-c(1,10)])
#conf_matrix_svm <- table(svm_predict,test$is_diabetic)
#confusionMatrix(conf_matrix_svm)
library(ROCR)
pr_svm <- prediction(svm_predict,test$is_diabetic)
prf_svm <- performance(pr_svm,measure = "tpr",x.measure = "fpr")
plot(prf_svm)
default.pred_svm <- ifelse(svm_predict > 0.3,1,0)
conf_matrix_svm <- table(default.pred_svm,test$is_diabetic)
confusionMatrix(conf_matrix_svm)


# KNN model

normalize <- function(x){
     return ((x-min(x))/(max(x)-min(x)))
}
norm_train <- as.data.frame(lapply(train, normalize))
norm_test <- as.data.frame(lapply(test, normalize))
model_knn <- train(norm_train[,-c(1,10)],as.factor(norm_train[,10]),method = 'knn')
predict_knn <- predict(object = model_knn, norm_test[,-c(1,10)])
table(predict_knn)
confusionMatrix(predict_knn,norm_test[,10])

# random forest model

install.packages('randomForest')
library('randomForest')
rf <- randomForest(as.factor(is_diabetic)~.,ntree = 100, data = train[-1])
rf
varImpPlot(rf,sort = T,n.var = 9,main = "Important Variables")
predict_rf <- predict(rf,test[-1])
View(predict_rf)
conf_martix_rf <- table(predict_rf, test$is_diabetic)
confusionMatrix(conf_martix_rf)

# Neural Networks

install.packages('nnet')
library('nnet')
ideal <- class.ind(pick_labs1$is_diabetic)


# GBM 

install.packages('gbm')
library('gbm')
gbm_model = gbm(formula = is_diabetic~.,distribution = "bernoulli",data = train,n.trees = 2500,shrinkage = .01,n.minobsinnode = 20)
predict_gbm <- predict(object = gbm_model,newdata = test[-10],n.trees = 1500,type = "response")
conf_matrix_gbm <- data.frame("Actual"= test$is_diabetic, "predicted" = predict_gbm)
library(ROCR)
pr_gbm <- prediction(predict_gbm,test$is_diabetic)
prf_gbm <- performance(pr_gbm,measure = "tpr",x.measure = "fpr")
plot(prf_gbm)
default.pred_gbm <- ifelse(predict_gbm > 0.4,1,0)
conf_matrix_gbm1 <- table(default.pred_gbm,test$is_diabetic)
confusionMatrix(conf_matrix_gbm1)