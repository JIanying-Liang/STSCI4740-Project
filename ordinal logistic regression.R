cancer_data_new<-cancer_data[,-1]
summary(cancer_data_new)
#cancer_data_new[which(cancer_data_new$Level=='Low'),24]<-0
#cancer_data_new[which(cancer_data_new$Level=='Medium'),24]<-0.5
#cancer_data_new[which(cancer_data_new$Level=='High'),24]<-1
cancer_data_new_2<-cancer_data_new[,2:24]
cancer_data_new_2<-apply(cancer_data_new_2,2,factor)
cancer_data_new<-cbind(cancer_data[,2],cancer_data_new_2)

#Ordinal logistic regression
set.seed(1)
training_index<-sample(1:nrow(cancer_data_new),0.8*nrow(cancer_data_new))
#glm.fit<-glm(Level~.,data = cancer_data_new[training_index,],family = binomial)

library(foreign)
library(MASS) 
m<-polr(Level~.,data = cancer_data_new[training_index,])

#Multinomial logistic regression
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

cancer_data_new$Level<-relevel(cancer_data_new$Level,ref = "Low")
ml<-multinom(Level~.,data = cancer_data_new,subset = training_index)
summary(ml)
ml.fit<-predict(ml,cancer_data_new[-training_index,])
mean((ml.fit!=apply(cancer_data_new[-training_index,],2,factor))^2)

library(leaps)
sum(is.na(cancer_data_new))
cancer_data_new<-na.omit(cancer_data_new)
regfit.full=regsubsets(Level~.,cancer_data_new) #all subset selection 
summary(regfit.full)

cancer_data_train<-cancer_data_new[training_index,]
cancer_data_test<-cancer_data_new[-training_index,]
x=model.matrix(Level~.,cancer_data_train)[,-1]
new_x<-model.matrix(Level~.,cancer_data_test)[,-1]
y<-cancer_data_train$Level
library(glmnet)
cvfit = cv.glmnet(x,y,type.measure="class",alpha=1,family="multinomial")
coef(cvfit, s = "lambda.min")
bestlam=cvfit$lambda.min
lasso.mod=glmnet(x,y,alpha=1,lambda=bestlam,family="multinomial")
pred.lasso = predict(lasso.mod, s = bestlam, newx = new_x,type = "response")
predicted <- colnames(pred.lasso)[apply(pred.lasso,1,which.max)]

mean(predicted!=cancer_data_test$Level)



