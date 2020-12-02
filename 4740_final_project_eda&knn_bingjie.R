library(ggplot2)
install.packages("readxl")
library("readxl")
data <-read_excel("C:/Users/olive/Desktop/4740/cancer_data.xlsx")
typeof(data)
df <- data.frame(data)
head(df)

nrow(df)
ncol(df)
dim(df)

head(df,4)
tail(df,4)

summary(df)

str(df)

fix(df)

View(df)

data$Level <- as.factor(df$Level)
data$Level


table(df$Level)
magnitude.counts <- table(df$Level)

#checking missing value:
sum(is.na(df))
#no missing value


install.packages("skimr")
library(skimr)
skim(df)

#group data by Levels then perform skim
df %>%
    dplyr::group_by(Level) %>%
    skim()


##########
#Data visualization


#scatter
plot(df$Age,df$Gender,col='red',xlab='Age',ylab='Gender')

#histogram
hist(df$Age,col='blue')

#boxplot
boxplot(df$Age, df$Gender, df$Air.Pollution,df$Alcohol.use,df$Dust.Allergy, df$OccuPational.Hazards, df$Genetic.Risk,df$chronic.Lung.Disease,
        main = '',
        names = c('Age','Gender','Air Pollution','Alcohol use','Dust Allergy','OccuPational Hazards','Genetic Risk','chronic Lung Disease'),
        col = 'green'
)



boxplot(df$Balanced.Diet, df$Obesity, df$Smoking,df$Passive.Smoker,df$Chest.Pain,df$Coughing.of.Blood,df$Fatigue,df$Weight.Loss,
        main = '',
        names = c('Balanced Diet','Obesity','Smoking','Passive Smoker','Chest Pain','Coughing of Blood','Fatigue','Weight Loss'),
        col = 'green'
)

boxplot(df$Shortness.of.Breath, df$Wheezing, df$Swallowing.Difficulty,df$Clubbing.of.Finger.Nails,df$Frequent.Cold,df$Dry.Cough,df$Snoring,
        main = '',
        names = c('Shortness of Breath','Wheezing','Swallowing Difficulty','Clubbing of Finger Nails','Frequent Cold','Dry Cough','Snoring'),
        col = 'green'
)

#potential outliers
dplyr::filter(df, Age>70)

boxplot.stats(df$Age)$out

###new add:

colMeans(df[,c(2:24)])

#find the covariance among variables:
cov(df[,c(2:24)])

#find the correlation among variables:
cor(df[,c(2:24)])

MD <- mahalanobis(df[,c(2:24)],colMeans(df[,c(2:24)]),cov(df[,c(2:24)]))

df$MD <-round(MD,3)
head(df)
MD[1:200] %>% round(2)

#Mahalanobis outliers (set to 50)
df$outlier_maha <- FALSE
df$outlier_maha[df$MD >50] <- TRUE

head(df)

summary(df)
#Therefore, based on the summary(df), we have 30 data which could be outliers.
#Those are:
dplyr::filter(df, outlier_maha == TRUE)

#Then drop those outliers:
df_clean<- df[-which(df$outlier_maha ==TRUE),]
df_clean 
dim(df_clean)
df_clean <-df_clean[,1:25] # the dataset after dropping outliers
dim(df_clean)
############

# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,] #X for training data
test.X=cbind(Lag1,Lag2)[!train,] #X for testing data
train.Direction=Direction[train] #Y for training data
set.seed(1) # to ensure the reproducibility of the results, I suggest to set a random seed before the analysis. 
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

#KNN:
set.seed(1)
##Generate a random number that is 80% of the total number of rows in data set.
ran <- sample(1:nrow(df_clean), 0.8 * nrow(df_clean)) 
ran

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run normalization on the 2nd-24th columns of data set because they are the predictors
data_norm <- as.data.frame(lapply(df_clean[,c(2:24)], nor))

summary(data_norm)

##extract training set
data_train <- data_norm[ran,] 
##extract testing set
data_test <- data_norm[-ran,] 

#extract 25th column of train dataset because it will be used as 'cl' argument in knn function.
data_target_category <- df_clean[ran,25]

##extract 25th column if test dataset to measure the accuracy
data_test_category <- df_clean[-ran,25]

##load the package class
library(class)
##run knn function
pr <- knn(data_train,data_test,cl=data_target_category,k=50)

##create confusion matrix
tab <- table(pr,data_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#In the data, I have run the k-nearest neighbor algorithm that gave me 95.87629% accurate result. 

#First, I normalized the data to convert the 2nd-24th columns of variables into a standardized 0-to-1 form so that we can fit them into one box (one graph) ;
#the main objective is to predict whether a level is low, median, or high and that is why I excluded the column 25 and stored it into another variable called data_target_category. 
#Then, I separated the normalized values into training and testing dataset. Imagine it this way, that the values from training dataset are firstly drawn on a graph and after we run knn function with all the necessary arguments, 
#we introduce testing dataset’s values into the graph and calculate Euclidean distance with each and every already stored point in graph. 
#Now, although we know which level it is in testing dataset, we still predict the values and store them in variable called ‘pr’ so that we can compare predicted values with original testing dataset’s values. 
#This way we understand the accuracy of our model.

