#################################################################### 区间整理
data<- read.table("hs_chrY_AZF_STS.list.multi", colClass="character")
region<- t(apply(data, 1, function(x) as.numeric(unlist(strsplit(x[1] , split=":|-|\\|"))[2:3])))
data<- data[ order(region[,1]) ,]
region<- region[order(region[,1]) ,]

cross<- NULL
for(i in 2:dim(region)[1])
{
	if(region[i,1] <= region[i-1,2]) cross<- c(cross , i)
}

region.length<- unlist( apply(region, 1, function(x) x[1]:x[2]) )

merged.region<- rbind(0, region[1,])
t<-NULL
for(i in 2:dim(region)[1])
{
	last.row<- dim(merged.region)[1]
	if( region[i,1] <= merged.region[last.row, 2] )
		merged.region[last.row,]<- c( merged.region[last.row , 1] , max(merged.region[last.row , 2] , region[i , 2]))
	else
		merged.region<- rbind(merged.region , region[i,])
	if(i == dim(region)[1]) merged.region<- merged.region[-1,]
}

#################################################################### data normalization
path<- "C:/Documents and Settings/admin/My Documents/测序深度估计/merged_region_all_reads/"
depth<- NULL
file<- dir(path , full.name=T)
for(i in file)
{
	depth<- cbind(depth , read.table(i)[,3])
	#name<- gsub("_ununique.txt", "", i)
	#png(paste("C:/Documents and Settings/admin/My Documents/测序深度估计/plot/", name, ".png", sep=""), height=600, width=800, units="px", pointsize=12)
	#	par(mar=c(5,5,4,2))
	#	data<- read.table( paste(path, i, sep="") )[,3]
	#	hist(data, breaks=60, col="lightgray", xlab="Region mean depth", main=list(paste("Sample", name, sep=" ") , cex=2), cex.axis=2, cex.lab=2, mar=c(5,5,4,2))
	#dev.off()
}

png("region_depth.png" , height=600, width=800, units="px", pointsize=12)
par(mfcol=c(1 , 2), mar=c(5,5,4,2))
	hist(region.depth , breaks=40, col="lightgray", cex.axis=2, cex.lab=2, xlab="Region Median depth", main="")
	boxplot(region.depth, col="lightgray", cex.axis=2, cex.lab=2, xlab="Region Median depth")
dev.off()

png("sample_depth.png" , height=600, width=800, units="px", pointsize=12)
par(mfcol=c(1 , 2), mar=c(5,5,4,2))
	hist(sample.depth , breaks=40, col="lightgray", cex.axis=2, cex.lab=2, xlab="Sample Median depth", main="")
	boxplot(sample.depth, col="lightgray", cex.axis=2, cex.lab=2, xlab="Sample Median depth")
dev.off()

#################################################################### 样本深度分布相关性挖掘
depth<- rbind(1:dim(depth)[2] , depth)
sample.correlation<- unlist(apply(cbind(1:dim(depth)[2] , cor(depth))[-1,] , 1 , function(x){
			x[2:x[1]]
}))

region.correlation<- unlist(apply(cbind(1:dim(depth)[1] , cor(t(depth)))[-1,] , 1 , function(x){
			x[2:x[1]]
}))

png("sample_correlation.png", height=600, width=800, units="px", pointsize=12)
#par(mar=c(5,5,4,2)
#hist(region.correlation , 100, xlab="Pearson correlation", ylab="Density", cex.axis=2, cex.lab=2, main="", prob=T)
myImagePlot(cor(depth) , zlim=c(-1,1))
dev.off()

adjacent.correlation<- diag(cor(t(depth))[2:dim(depth)[1]-1, 2:dim(depth)[1]])
png("adjacent_region_correlation.png", height=600, width=800, units="px", pointsize=12)
par(mar=c(5,5,4,2))
boxplot(adjacent.correlation, xlab="", ylab="Pearson correlation", cex.axis=2, cex.lab=2)
dev.off()


#################################################################### 统计检验
#===================|
# high quality data	|
#===================|
sample.depth<- apply(depth , 2 , median)
region.depth<- apply(depth , 1 , median)

region.quantile<- c(2.5*quantile(region.depth, 0.25) - 1.5*quantile(region.depth, 0.75) , 2.5*quantile(region.depth, 0.75) - 1.5*quantile(region.depth, 0.25))
sample.quantile<- c(2.5*quantile(sample.depth, 0.25) - 1.5*quantile(sample.depth, 0.75) , 2.5*quantile(sample.depth, 0.75) - 1.5*quantile(sample.depth, 0.25))

region<- which(region.depth > 0 & region.depth < region.quantile[2])
sample<- which(sample.depth > sample.quantile[1] & sample.depth < sample.quantile[2])
data<- depth[region , sample]

#========================================|
# sample weighted random normalization	|
#========================================|
sample.sum.depth<- apply(data , 2 , sum)
weight<- apply(data , 1 , median)/sum(apply(data , 1 , median))
normalize<- NULL
for(i in 1:dim(data)[2])
{
	sampling<- sample(1:dim(data)[1] , max(sample.sum.depth) - sample.sum.depth[i] , prob=weight, replace=T)
	normalize<- cbind(normalize , data[,i] + as.numeric(table(factor(sampling , levels=1:dim(data)[1]))))
}

#===============================================|
# normal test						|
# 发现样本"w362_ununique.txt", data[,385]异常	|
#===============================================|
data<- depth[region , sample]
normalize<- sweep(data , 2 , apply(data , 2 , mean), "/")
p.value<- t(apply(normalize, 1, function(x){
			cutoff<- c(2.5*quantile(x, 0.25) - 1.5*quantile(x, 0.75) , 2.5*quantile(x, 0.75) - 1.5*quantile(x, 0.25))
			y<- x[x>cutoff[1] & x<cutoff[2]]
			pnorm(x , mean(y) , sd(y))
}))
p.value<- c(p.value)

#==========================|
# regression residual test	|
#==========================|
fit.data<- cbind( c(depth) , rep(colMeans(depth), each=dim(depth)[1]) , rep(rowMeans(depth), times=dim(depth)[2]) )
fit.data<- fit.data[fit.data[,1]!=0 & fit.data[,1]<154.9082 , ]
regression<- glm(fit.data[,1] ~ fit.data[,2] + fit.data[,3], family="gaussian")
residual<- regression[[2]]
all.residual<- fit.data[,1] - (-56.230576 + 1.305795*fit.data[,2] + 0.821582*fit.data[,3])
cutoff<- c(2.5*quantile(residual, 0.25) - 1.5*quantile(residual, 0.75) , 2.5*quantile(residual, 0.75) - 1.5*quantile(residual, 0.25))
p.value<- pnorm(all.residual , mean(residual[residual>cutoff[1] & residual<cutoff[2]]) , sd(residual[residual>cutoff[1] & residual<cutoff[2]]))

png("normal_p_value.png", height=800, width=1600, units="px", pointsize=12)
par(mfcol=c(1, 2) , mar=c(5,5,4,2))
plot(-log10(p.value), pch=20, cex=1.5, xlab="", ylab=expression(-log[10](p-value)), cex.axis=2, cex.lab=2, xaxt="n", col=rep(1:485, each=767)%%4 + 1)
qqplot(rnorm(length(p.value) , mean(p.value), sd(p.value)) , -log10(p.value), xlab="Expected normal p-value", ylab="Observed p-value", cex.axis=2, cex.lab=2)
	abline(a=0, b=pi/4, col=3, lwd=2)
dev.off()

png("p_value.png", height=600, width=800, units="px", pointsize=12)
par(mar=c(5,5,4,2))
plot(-log10(c(z, y)), pch=20, col=rep(1:2, each=length(y)), xlab="", ylab=expression(-log[10](p-value)), xaxt="n", cex.axis=2, cex.lab=2)
dev.off()

#=====================================================|
# significant p-value overlap between the two method	|
#=====================================================|
overlap<- NULL
for(i in 1:22)
{
	index1<- which(-log10(z) >= i)
	index2<- which(-log10(y) >= i)
	overlap<- rbind(overlap , c(i , length(intersect(index1 , index2))))
}

png("overlap.png", height=600, width=800, units="px", pointsize=12)
par(mar=c(5,5,4,2))
plot(overlap[-(1:4),1], overlap[-(1:4),2], xlim=c(5, 22), type="o", cex=2, pch=4, lwd=2, xlab=expression(-log[10](p-vlaue)), ylab="Overlap Number", cex.axis=2, cex.lab=2)
dev.off()

#################################################################### 缺失规律挖掘以及验证
p.value<- read.table("p_value.txt")
p.value.cutoff<- 9
depth.cutoff<- max(p.value[,3])
positive<- which(-log10(p.value[,6]) >= p.value.cutoff & p.value[,3]<= depth.cutoff)
#negative<- sample(which(-log10(p.value[,6]) < 9) , length(case.index))
#training.data<- rbind(as.matrix(cbind(1, p.value[case.index, 3:5])) , as.matrix(cbind(0, p.value[control.index, 3:5])))

data<- p.value[positive,]
sample<- names(table(as.matrix(data)[,1]))
region<- names(table(as.matrix(data)[,2]))
index<- match(paste(rep(sample, each=length(region)), rep(region, length(sample)), sep=":") , paste(p.value[,1], p.value[,2], sep=":"))
depth<- matrix(p.value[index,3], nr=length(sample), byrow=T)
#p<- matrix(-log10(p.value[index,6]), nr=length(sample), byrow=T)
#col<- matrix(ifelse(-log10(p.value[index,6])>=p.value.cutoff, 2, 3), nr=length(sample), byrow=T)
col<- matrix(ifelse( sample.fix[index] < 0.1775 & region.fix[index] < 0.5, 2, 3), nr=length(sample), byrow=T)
rownames(depth)<- sample
colnames(depth)<- region
image.plot(depth , col=col)


setdir("C:\Users\Administrator\Desktop\测序深度估计")
p.value<- read.table("p_value.txt")
x<- -log10(p.value[,6])
y<- -log10(p.value[,7])
x.sig<- which(x >= rev(sort(x))[400])
y.sig<- which(y >= rev(sort(y))[400])

png("significant_unique.png", height=400, width=800, units="px", pointsize=12)
par(mfcol=c(1,2) , mar=c(5,5,4,2))
#index<- setdiff(y.sig , x.sig)
hist(p.value[index,3] , 40, xlab="Depth", cex.axis=2, cex.lab=2, main="")
	col.factor<- as.numeric(factor(p.value[index, 1] , labels=1:length(unique(p.value[index, 1]))))%% 4 + 1
	x<- -log10(p.value[,6])
plot(x[index], pch=20, cex=1.5, col=col.factor, xlab="", ylab=expression(-log[10](p-value)), xaxt="n", cex.axis=2, cex.lab=2)
#plot(y[index], pch=20, cex=2, col=col.factor, xlab="", ylab=expression(-log[10](p-value)), xaxt="n", main=list("Method2", cex=2), cex.axis=2, cex.lab=2)
dev.off()

#################################################################### 训练数据
path<- "C:/Documents and Settings/admin/My Documents/测序深度估计/training sample/"
data<- read.table(paste(path, "training_sample.list", sep=""),  colClass="character")
region<- as.character(data[1, -(1:3)])
index<- order(as.numeric(gsub(".+:|-.+", "", region)))
sample<- data[-1,]
depth<- NULL
file<- dir(path, full.name=T)
for(i in 1:dim(sample)[1])
{
	if(!T %in% grepl(sample[i,3] , file))
		next
	data<- read.table(file[grep(sample[i,3] , file)])
	#depth<- rbind( depth , c(as.character(sample[i,1]) , data[match(region , data[,1]),3]))
	depth<- rbind( depth , data[match(region , data[,1]),3])
}

sample.median<- apply(depth , 2 , median)
region.median<- apply(depth , 1 , function(x) quantile(x, 0.75))
regression<- glm(fit.data[,1] ~ fit.data[,2] + fit.data[,3], family="gaussian")
residual<- fit.data[,1] - (-56.230576 + 1.305795*fit.data[,2] + 0.821582*fit.data[,3])
cutoff<- c(2.5*quantile(residual, 0.25) - 1.5*quantile(residual, 0.75) , 2.5*quantile(residual, 0.75) - 1.5*quantile(residual, 0.25))
p.value<- pnorm(residual , mean(residual[residual>cutoff[1] & residual<cutoff[2]]) , sd(residual[residual>cutoff[1] & residual<cutoff[2]]))
fit.data<- cbind( c(depth) , rep(sample.median, each=dim(depth)[1]) , rep(region.median, times=dim(depth)[2]) )


file<- dir()
file<- file[grep("^110902|w|W" , file)]
depth<- NULL
for(i in file)
{
	depth<- cbind(depth , read.table(i)[,3])
}
region<- read.table(file[1], colClass="character")[,1]
data<- depth
normalize<- sweep(data , 2 , apply(data , 2 , mean), "/")
p.value<- t(apply(normalize, 1, function(x){
			cutoff<- c(2.5*quantile(x, 0.25) - 1.5*quantile(x, 0.75) , 2.5*quantile(x, 0.75) - 1.5*quantile(x, 0.25))
			y<- x[x>cutoff[1] & x<cutoff[2]]
			pnorm(x , mean(y) , sd(y))
}))
write.table(cbind(c("sample", region), rbind(file, p.value)), "p.value",  quote=F, row.names=F, col.names=F, sep="\t")

path<- "C:/Documents and Settings/admin/My Documents/测序深度估计/training sample/"
data<- read.table(paste(path, "training_sample.list", sep=""),  colClass="character")
region<- as.character(data[1, -(1:3)])
sample<- data[-1,3]

p.value<- read.table("p.value", colClass="character")
sample.p<- as.character(p.value[1,])
region.p<- as.character(p.value[,1])
sample.index<- NULL
for(i in sample)
{
	sample.index<- c(sample.index , grep(i , sample.p))
}
region.index<- match(region , region.p)
sample.p.value<- t(p.value[region.index , sample.index])
write.table(rbind(c("lab", region) , cbind(sample,sample.p.value)) , "sample_p_value.txt", quote=F, row.names=F, col.names=F, sep="\t")

deletion<- read.table("training_sample.list", skip=1)[,-(1:3)]
p.value<- read.table("sample_p_value.txt", skip=1)[,-1]
sample<- as.character(read.table("training_sample.list", skip=1)[-1,3])
region<- as.character(read.table("training_sample.list", skip=1)[1,-(1:3)])
test<- cbind(c(as.matrix(deletion)), -log10(c(as.matrix(p.value))))
test<- test[!is.na(test[,2]),]

ROC<- NULL
case<- which(test[,1]==1)
control<- which(test[,1]==0)
for(i in 0:round(max(test[,2])))
{
	FP<- length(which(test[control,2]>=i)) / length(control)
	TP<- length(which(test[case,2]>=i)) / length(case)
	ROC<- rbind(ROC , c(i, TP, FP))
}

AUC<- 0
for(i in 2:dim(ROC)[1])
{
	AUC<- AUC + (ROC[i,2] + ROC[i-1,2])*(ROC[i-1,3] - ROC[i,3])/2
}

#################################################################### 决策树 and SVM
training.sample<- read.table("sample_median.txt")
training.region<- read.table("region_median.txt")
region.fixed<- c(sweep(depth[-13,] , 2 , training.region[,1] , "/"))
sample.fixed<- c(sweep(depth[-13,] , 1 , training.sample[-13,2] , "/"))
training.data<- cbind(rep(sample[-13,1], length(region)) , rep(region, each=dim(sample)[1]-1) , region.fixed , sample.fixed, c(as.matrix(sample[-13,-(1:3)])))
write.table(training.data , "training_data.list", quote=F, row.names=F, col.names=F, sep="\t")

p.value<- read.table("p_value.txt")
depth<- matrix(p.value[,3], nr=length(table(p.value[,2])))
region.median<- apply(depth , 1 , median)
sample.median<- apply(depth , 2 , median)
region.fix<- p.value[,3]/rep(region.median , 767)
sample.fix<- p.value[,3]/rep(sample.median , each=485)

positive<- which(sample.fix < 0.1775 & region.fix < 0.5)