data.matrix <- matrix(nrow=100,ncol = 10)
colnames(data.matrix) <- c(paste('wt',1:5,sep = ''),paste('ko',1:5,sep = ''))
rownames(data.matrix) <- c(paste('gene',1:100,sep=''))
for (i in 1:100){
  wt.values <- rpois(5,lambda = sample(x=10:1000,size = 1))
  ko.values <-rpois(5,lambda = sample(x=10:1000,size=1))
  data.matrix[i,] <- c(wt.values,ko.values)
} 
pca <- prcomp(t(data.matrix),scale. = TRUE,center = TRUE)#prcomp默认希望样本是行，变量是列。所以这里先对数据矩阵进行了转置
#prcomp返回值：sdev主成分标准差，表示每个主成分所解释的变异程度。rotation：每个原始变量对主成分的贡献。x：变换后的数据矩阵。包含主成分得分。center数据中心的均值。scale数据标准差。
plot(pca$x[,1],pca$x[,2])#为了画一个2d的图，用前两个主成分。
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,2)
barplot(pca.var.per,main = "Screet Plot",xlab = 'PC',ylab = 'Percent Variation')#凯瑟森图
library(ggplot2)
pca.data <- data.frame(Sample=rownames(pca$x),X=pca$x[,1],Y=pca$x[,2])
pca.data
ggplot(data = pca.data,aes(x=X,y=Y,label=Sample))+geom_text()+xlab(paste('PC1 - ',pca.var.per[1],'%',sep = ''))+ylab(paste('PC2 - ',pca.var.per[2],'%',sep = ''))+theme_bw()+ggtitle('PCA Graph')
#查看哪个变量对主成分贡献最大
loading_scores <- pca$rotation[,1]#每个主成分的loading scores。这里只看主成分1的
gene_scores <- abs(loading_scores)
gene_score_ranked <- sort(gene_scores,decreasing = TRUE)
top_10_genes <- names(gene_score_ranked[1:10])
