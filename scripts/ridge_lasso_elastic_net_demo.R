library(glmnet)
set.seed(42)
n <- 1000
p <- 5000
real_p <- 15 #只有15个变量是真实有用的
x <- matrix(rnorm(n*p),nrow = n,ncol = p)
y <- lapply(x[,1:real_p], 1,sum)+rnorm(n)#apply:主要用于矩阵（或二维数组）。可以沿着指定维度应用一个函数。lapply：主要用于列表，列表的每个元素应用一个函数。