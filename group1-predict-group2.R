###########这个流程是训练集和测试集都知道类型的情况下
#是否需要测试集进行验证根据自己需求判断
#如果不需要测试测试集，直接把group2全部换成group1即可
rm(list = ls())
#读入实验设计表
design = read.table("design.txt",header = T, row.names = 1)
#读取数据
otu_table =read.table("pfam.txt",header = T, row.names = 1)
#筛选group1数据为训练集建立模型
design_sub = subset(design, Group %in% c("group1"))
design_sub$Type <- as.factor(design_sub$Type)
idx = rownames(design_sub) %in% colnames(otu_table)
design_sub = design_sub[idx,]
otu_sub = otu_table[, rownames(design_sub)]
library(randomForest)
#设置随机种子
set.seed(315)
df = t(otu_sub)
df <- as.data.frame(df)
df <- as.data.frame(lapply(df,as.numeric))
df <- cbind(df,Type=design_sub$Type)
# rf = randomForest(Type~., data = df, importance=TRUE, proximity=TRUE, ntree = 1000)
rf = randomForest(t(otu_sub), design_sub$Type, importance=TRUE, proximity=TRUE, ntree = 1000)
print(rf) #这里会显示训练集结果，可自己选择是否保存


#交叉验证
set.seed(315) # 保证结果可重复
result = rfcv(t(otu_sub), design_sub$Type, cv.fold=5)
result$error.cv
# 绘制验证结果 
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

imp= as.data.frame(rf$importance)
imp = imp[order(imp[,1],decreasing = T),]

head(imp,n=10)
write.table(imp,file = "importance_class222.txt",quote = F,sep = '\t', row.names = T, col.names = T)
# 简单可视化
varImpPlot(rf, main = "Top 10 - Feature importance",n.var = 10, bg = par("bg"), color = par("fg"), gcolor = par("fg"), lcolor = "gray" )

#筛选
design_test = subset(design, Group %in% c("group2")) 
summary(design_test)
idx = rownames(design_test) %in% colnames(otu_table)
design_test = design_test[idx,]
otu_sub = otu_table[,rownames(design_test)]

#转置，并添加分组信息
otutab_t = as.data.frame(t(otu_sub))
otutab_t$Type = design[rownames(otutab_t),]$Type

set.seed(315)
otutab.pred = predict(rf, t(otu_sub) )  
pre_tab = table(observed=otutab_t[,"Type"],
                predicted=otutab.pred) 

predict = data.frame(Type = otutab_t[,"Type"], predicted=otutab.pred)
print(predict)
#保存预测结果
write.table("SampleID\t", file=paste("RF_prediction_binary3333333.txt",sep=""),append = F, quote = F, eol = "", row.names = F, col.names = F)
write.table(predict, file = "RF_prediction_binary3333333.txt",append = T, quote = F, row.names = T, col.names = T, sep = "\t")

