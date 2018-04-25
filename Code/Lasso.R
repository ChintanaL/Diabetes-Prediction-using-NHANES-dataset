# Lasso Regularisation using GLMNET
install.packages("glmnet")
library(glmnet)


finalLasso<- read.csv("C:/Users/chintana/OneDrive/Masters/Projects/Dataset/Cleant/finalexamDemoDiet.csv", stringsAsFactors = F)
#View(final)
str(finalLasso)

finalLasso<-finalLasso[,-c(1,2)] # removng SEQN

set.seed(123)
trainLassoInd <- createDataPartition(finalLasso$is_diabetic, p=0.80, list=FALSE)
nrow(finalexamDemoDietNUM)

trainLasso = finalLasso[trainLassoInd,]
testLasso = finalLasso[-trainLassoInd,]
nrow(trainLasso)
nrow(testLasso)
colnames(trainLasso)

x <- model.matrix(is_diabetic~.,trainLasso)

#perform grid search to find optimal value of lambda
#family= binomial => logistic regression,
# alpha=1 => lasso


cv.out <- cv.glmnet(x,trainLasso$is_diabetic,alpha=1,family='binomial',type.measure = 'mse' )
  #plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)


# Output: is 
#AgeYears                    0.06899236248016
#SAD                         0.12353175939840
#DR1BWATZ                    0.00001964007514
#DR1TALCO                   -0.00218434634431
#DR1TCARB                   -0.00023578289817

#get test data
x_test <- model.matrix(is_diabetic~.,testLasso)
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type='response')
nrow(lasso_prob)
#translate probabilities to predictions
#lasso_predict <- rep(0,nrow(testLasso))
#nrow(lasso_predict)

lasso.pred <- ifelse(lasso_prob > 0.5,1,0)


#lasso_predict[lasso_prob>.5] <- 1
#confusion matrix
nrow(testLasso)
nrow(lasso.pred)
lassot<-table(pred=lasso.pred,true=testLasso$is_diabetic)
mean(lasso.pred==testLasso$is_diabetic)

confusionMatrix(lassot)
### Accuracy 86.69