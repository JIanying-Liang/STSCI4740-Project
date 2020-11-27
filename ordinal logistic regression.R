cancer_data_new<-cancer_data[,-1]
summary(cancer_data_new)
#cancer_data_new[which(cancer_data_new$Level=='Low'),24]<-0
#cancer_data_new[which(cancer_data_new$Level=='Medium'),24]<-0.5
#cancer_data_new[which(cancer_data_new$Level=='High'),24]<-1
cancer_data_new$Age<-as.factor(cancer_data_new$Age)
cancer_data_new$Gender<-as.factor(cancer_data_new$Gender)
cancer_data_new$`Air Pollution`<-as.factor(cancer_data_new$`Air Pollution`)

for (i in 1:24){
  cancer_data_new[,i]<-as.factor(cancer_data_new[,i])
}
set.seed(1)
training_index<-sample(1:nrow(cancer_data_new),0.8*nrow(cancer_data_new))
#glm.fit<-glm(Level~.,data = cancer_data_new[training_index,],family = binomial)

library(foreign)
library(MASS)
m<-polr(Level~.,data = cancer_data_new[training_index,])
