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



