library(glmnet)
library(esquisse)
library(ggThemeAssist)
set.seed(42)
n <- 1000
p <- 5000
real_p <- 15 #只有15个变量是真实有用的
x <- matrix(rnorm(n*p),nrow = n,ncol = p)
y <- apply(x[,1:real_p], 1,sum)+rnorm(n)#apply:主要用于矩阵（或二维数组）。可以沿着指定维度应用一个函数。lapply：主要用于列表，列表的每个元素应用一个函数。维度为1的话就是对列进行求和。
train_rows <- sample(1:n,.66*n)#sample随机选择数字1:n
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]
#ridge regression
alpha0.fit <- cv.glmnet(x_train,y_train,type.measure = 'mse',alpha=0,family='gaussian')
#这里cv默认的是10择。如果想用Logistics regression这里把mse改为deviance。family = 'gaussion'表示这里doing 线性回归如果我们正在做Logistics regression这里改为binomial
alpha0.predicted <- predict(alpha0.fit,s=alpha0.fit$lambda.1se,newx = x_test)
mean((y_test-alpha0.predicted)^2)
#lasso regression
alpha1.fit<- cv.glmnet(x_train,y_train,type.measure = 'mse',alpha=1,family='gaussian')
alpha1.predicted <- predict(alpha1.fit,s=alpha1.fit$lambda.1se,newx = x_test)
mean((y_test-alpha1.predicted)^2)
#elastic net
alpha0.5.fit <- cv.glmnet(x_train,y_train,type.measure = 'mse',alpha=0.5,family='gaussian')
alpha0.5.predicted <- predict(alpha0.5.fit,s=alpha0.5.fit$lambda.1se,newx = x_test)
mean((y_test-alpha0.5.predicted)^2)
#创建一个空列表存储一系列elastic net结果
list_of_fits <- list()
for (i in 0:10){
  fit.name <- paste0('alpha',i/10)
  list_of_fits[[fit.name]] <- cv.glmnet(x_train,y_train,type.measure = 'mse',alpha=i/10,family='gaussian')
}
results <- data.frame()
for (i in 0:10){
  fit.name <- paste0('alpha',i/10)
  predicted <- predict(list_of_fits[[fit.name]],s=list_of_fits[[fit.name]]$lambda.1se,newx = x_test)
  mse <- mean((y_test-predicted)^2)
  temp <- data.frame(alpha=i/10,mse=mse,fit.name=fit.name)
  results <- rbind(results,temp)
}