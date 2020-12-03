cancer_data = read.csv('D:/AmireuXFaye/Cornell/STSCI4740/cancer_data.csv')
cancer_data<-cancer_data[,-1]
View(cancer_data)
cancer_data$Level = as.factor(cancer_data$Level)

# Training set
set.seed(1)
train.index <- sample(1:nrow(cancer_data),0.8*nrow(cancer_data))
cancer_train <- as.matrix(cancer_data[train.index, 1:23])
cancer_te <- as.matrix(cancer_data[-train.index, 1:23])
level_train <- as.matrix(cancer_data[train.index, 24])
level_te <- as.matrix(cancer_data[-train.index, 24])

library(glmnet)

# Ridge
set.seed(1)
cv.out=cv.glmnet(cancer_train,level_train,alpha=0, family = "multinomial") 
ridge.bestlam=cv.out$lambda.min
ridge.mod=glmnet(cancer_train,level_train,alpha=0,lambda=ridge.bestlam, family = "multinomial")
# ridge.coef = coef(ridge.mod)[,1]
pred.ridge = predict(ridge.mod, s=ridge.bestlam, newx = cancer_te, type="response")
pred.level = colnames(pred.ridge)[apply(pred.ridge, 1, which.max)]
mse.ridge = mean((pred.level==level_te)^2)
mse.ridge # = 0.955

# Lasso
set.seed(1)
cv.out=cv.glmnet(cancer_train,level_train, type.measure = "class", alpha=1, family = "multinomial") 
lasso.bestlam = cv.out$lambda.min
lasso.mod = glmnet(cancer_train,level_train, type.measure = "class", alpha=1,lambda=lasso.bestlam, family = "multinomial")
pred.lasso = predict(lasso.mod, s=lasso.bestlam, newx = cancer_te, type = "response")
pred.level = colnames(pred.lasso)[apply(pred.lasso, 1, which.max)]
mse.lasso = mean((pred.level == level_te)^2)
mse.lasso # = 1




