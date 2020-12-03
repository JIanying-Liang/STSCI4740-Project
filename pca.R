#Packages
#install.packages('pls')
library(pls)
#install.packages("FactoMineR")
library("FactoMineR")
#install.packages("factoextra")
library("factoextra")
library(ggplot2)
#install.packages('class')
library(class)
#install.packages("readxl")
library("readxl")
#install.packages('dplyr')
library(dplyr)


#prepare data
data <- read.csv("cancer_data.csv")
data$Level = as.factor(data$Level)

##############################
# remove outliers (using code from Bingjie)

dplyr::filter(data, Age>70)

boxplot.stats(data$Age)$out

###new add:

colMeans(data[,c(2:24)])

#find the covariance among variables:
cov(data[,c(2:24)])

MD <- mahalanobis(data[,c(2:24)],colMeans(data[,c(2:24)]),cov(data[,c(2:24)]))

data$MD <-round(MD,3)
head(data)
MD[1:200] %>% round(2)

#Mahalanobis outliers (set to 50)
data$outlier_maha <- FALSE
data$outlier_maha[data$MD >50] <- TRUE

head(data)

summary(data)
#Therefore, based on the summary, we have 30 data which could be outliers.
#Those are:
dplyr::filter(data, outlier_maha == TRUE)

#Then drop those outliers:
data_clean<- data[-which(data$outlier_maha ==TRUE),]
data_clean 
dim(df_clean)
data <-data_clean[,1:25]

##############################

# principal components regression for preprocessing the data
set.seed(1)
data$Level = as.numeric(as.factor(data$Level))

cancer.pr <- prcomp(data[c(2:24)], center = TRUE, scale = TRUE)
summary(cancer.pr)
screeplot(cancer.pr, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(cancer.pr$sdev^2 / sum(cancer.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 9, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

#Firstly, from the summary and the cumulative variance plot, we can tell the percentage of variance explained in the 
#predictors and in the response using different numbers of components. When M=1, there is only 40.18% of all the variance been captured. 
#While when M=7, it increases to 82.229%. Till M=11, it increases to above 90%.

#In addition, from the scree plot of the eigenvalues, we can see that the first 6 components has an eigenvalue >1. 
#According to the Kaiser Rule, it says the more variables that load onto a particular component, the more important 
#the factor is in summarizing the data. An eigenvalue is an index that indicates how good a component is as a summary 
#of the data. An eigenvalue of 1.0 means that the factor contains the same amount of information as a single variable. 
#Even though there are some critiques about the Kaiser Rule, we choose the component number near 6.

#Consequently, we use M=11 component to compute the test MSE as follows, since it explains much variance and also has 
#relatively high eigenvalue. 


# Visualize the result using all features
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
  ggtitle("2D PCA-plot from 23 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))   

# Test MSE calculated using KNN method
scaling <- cancer.pr$sdev[1:11] * sqrt(nrow(data))
pc<-matrix(0,970,11)
for (i in 1:11) {
  pc[,i] <- rowSums(t(t(sweep(data[,2:24], 2 ,colMeans(data[,2:24]))) * cancer.pr$rotation[,i]) / scaling[i])
}
colnames(pc)<-c("pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10","pc11")
pc<-as.data.frame(pc)

set.seed(1)
training_index<-sample(1:nrow(pc),0.8*nrow(pc))
pc.train = pc[training_index,]
pc.te = pc[-training_index,]

data.train = data[training_index,]
data.te = data[-training_index,]
level.train <-data.train[,25]
level.te <- data.te[,25]

#k=10
knn.pred=knn(pc.train,pc.te,level.train,k=10)

table(knn.pred,level.te)
mean(knn.pred==level.te)

#k=20
knn.pred=knn(pc.train,pc.te,level.train,k=20)

table(knn.pred,level.te)
mean(knn.pred==level.te)

#It appears that if I use k=10, the accuracy is 89.175%. If k increases to 20, then the accuracy is 88.66%. 




