getwd()
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data'
data <- read.csv(url,header = FALSE)
colnames(data) <- c("age",'sex','cp','trestbps','chol','fbs','restecg','thalach','exang'
                    ,'oldpeak','slope','ca','thal','hd')
data[data=='?'] <- NA
data$sex <- ifelse(data$sex==0,'F','M')
factor_cols <- c('sex','cp','fbs','restecg','exang','slope')
data[factor_cols] <- lapply(data[factor_cols],as.factor)#R中离散变量是因子
data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)
data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)
data$hd <- ifelse(data$hd==0,'Healthy','Unhealthy')
data$hd <- as.factor(data$hd)
data <- data[!(is.na(data$ca)|is.na(data$thal)),]
#确定患有心脏疾病的人来自两个性别中，如果只有一个性别有心脏病，那么应该把另一个没有的删掉。
#建立一个列联表
xtabs(~hd+sex,data = data)
#是否是个等级的cp都有心脏病
xtabs(~hd+cp,data = data)
xtabs(~hd+fbs,data = data)
xtabs(~hd+restecg,data = data)
xtabs(~hd+exang,data = data)
logistic <- glm(hd~sex,data = data,family = 'binomial')
logistic1 <- glm(hd~.-age-restecg,data = data,family = 'binomial')
#AIC称为赤池信息量准则（Akaike Information Criterion），是一种用于模型选择的统计标准。它的主要目的是在一组候选模型中评估哪一个模型在解释数据方面表现更好，同时考虑到模型的复杂性.AIC的主要特点：
#AIC的主要特点：
# 模型比较：可以用来比较不同模型的拟合优度，选择AIC值最低的模型。
# 惩罚复杂性：比起简单的拟合优度测量，AIC在考虑模型拟合的同时，还惩罚过于复杂的模型（参数过多），以防止过拟合。
# 相对指标：AIC的绝对值没有意义，关键在于不同模型AIC值的比较。
predicted.data <- data.frame(probability.of.hd=logistic1$fitted.values,hd=data$hd)
prdicted.data <- predicted.data[order(predicted.data$probability.of.hd,decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)
ggplot(data = predicted.data,aes(x=rank,y=probability.of.hd))+geom_point(aes(color=hd),alpha=1,shape=4,stroke=2)+xlab('Index')+ylab("predicted probability of getting heart disease")
