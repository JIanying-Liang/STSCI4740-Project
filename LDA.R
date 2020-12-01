library(MASS)
cancer_data_new<-as.data.frame(cancer_data)
cancer_data_new<-cancer_data[,-1]
training_index<-sample(1:nrow(cancer_data),0.8*nrow(cancer_data))
lda.fit=lda(Level~.,data=cancer_data_new,subset=training_index)
lda.fit 
lda.pred=predict(lda.fit, cancer_data_new[-training_index,])
names(lda.pred)
lda.class=lda.pred$class

ls<-as.list(t(cancer_data_new[-training_index,24]))
ls2 <- as.data.frame(lapply(ls, unlist))
table(lda.class,ls2)
mean((lda.class==ls)^2)

#> mean((lda.class!=ls)^2)
#[1] 0.025
#> mean((lda.class==ls)^2)
#[1] 0.975