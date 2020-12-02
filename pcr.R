install.packages('pls')
library(pls)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library("factoextra")
library(ggplot2)

#data <- read.csv("cancer_data.csv")
data <- cancer_data[,-1]
data$Level = as.factor(data$Level)

set.seed(1)
data$Level = as.numeric(as.factor(data$Level))
pcr.fit=pcr(Level~., data=data,scale=TRUE,validation="CV") #standardize the covariate and use CV to choose M
summary(pcr.fit)  # M=16 has smallest CV error
validationplot(pcr.fit,val.type="MSEP")
cancer.pr <- prcomp(data[c(2:23)], center = TRUE, scale = TRUE)
summary(cancer.pr)
screeplot(cancer.pr, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(cancer.pr$sdev^2 / sum(cancer.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

data$Level = as.factor(data$Level)    
fviz_pca_ind(cancer.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = data$Level, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Level is") +
  ggtitle("2D PCA-plot from 22 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))   

set.seed(1)
training_index<-sample(1:nrow(data),0.8*nrow(data))
pcr.pred=predict(pcr.fit ,data[-training_index ,], ncomp =12)
data.te = data[-training_index,]
mean(as.factor(round(pcr.pred))==data.te$Level)


scaling <- cancer.pr$sdev[1:16] * sqrt(nrow(data))
pc<-matrix(0,1000,16)
for (i in 1:16) {
  pc[,i] <- rowSums(t(t(sweep(data[,2:23], 2 ,colMeans(data[,2:23]))) * cancer.pr$rotation[,i]) / scaling[i])
}
colnames(pc)<-c("pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10","pc11","pc12","pc13","pc14","pc15","pc16")
#pc1 <- rowSums(t(t(sweep(data[,2:23], 2 ,colMeans(data[,2:23]))) * cancer.pr$rotation[,1]) / scaling[1])
pc<-as.data.frame(pc)
