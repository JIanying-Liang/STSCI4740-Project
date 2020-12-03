cancer_data_new<-cancer_data[,-1]
summary(cancer_data_new)
cancer_data_model<-model.matrix(Level~.,cancer_data)[,-1]


#Multinomial logistic regression
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

training_index<-sample(1:nrow(cancer_data_model),0.8*nrow(cancer_data_model))
cancer_data_new$Level<-relevel(cancer_data_new$Level,ref = "Low")
ml<-multinom(Level~.,data = cancer_data_new,subset = training_index)
summary(ml)
ml.fit<-predict(ml,cancer_data_new[-training_index,])
mean((ml.fit!=apply(cancer_data_new[-training_index,],2,factor))^2)
#really bad result


library(glmnet)
set.seed(1)
training_index<-sample(1:nrow(cancer_data_model),0.8*nrow(cancer_data_model))
cancer_data_train<-cancer_data_model[training_index,]
cancer_data_test<-cancer_data_model[-training_index,]
y_train<-cancer_data$Level[training_index]
y_test<-cancer_data$Level[-training_index]
cvfit = cv.glmnet(cancer_data_train,y_train,type.measure="class",alpha=1,family="multinomial")
bestlam=cvfit$lambda.min
pred.lasso = predict(cvfit, s = bestlam, newx = cancer_data_test,type = "response")
predicted <- colnames(pred.lasso)[apply(pred.lasso,1,which.max)]
mean(predicted!=y_test)
#The cvfit contains 10*number of lambda training MSE and it automatically pick the smallest one which
#also corresponse with the smallest lambda. It provides the best lambda and best multinomial logistic regression model 
#using lasso.
#The final test MSE is 0.03.





