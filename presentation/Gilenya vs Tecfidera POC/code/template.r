######################################################### OR visualization of each sites
data<- read.table("T2D_中日.txt", header=T)
OR.CI<- cbind(data[,6] , matrix(unlist(strsplit(as.character(data[,7]) , split="-")), nc=2, byrow=T))
png("OR_visualization.png", height=600, width=800, units="px", pointsize=12)
par(mar=c(6,5,4,2)+0.1)
plot(NA, xlim=c(0, 30), ylim=c(1, 2), xaxt="n", cex.axis=2, xlab="", ylab="")
	mtext(side=2, line=3, "Odds Ratio(95% CI)", cex=2)
	par(las=2)
	axis(1, 0:30, data[,2], cex=1.5)
	for(i in 1:31)
	{
		points(i-1 , OR.CI[i,1], pch=15, cex=3)
		lines(rep(i-1 , 2), OR.CI[i,-1], type="o", pch=3, lwd=2)
	}
dev.off()
######################################################### data extract 31 risk SNP sites
ped<- read.table("HUMdurR_7520samples_96loci.ped", sep="\t")
map<- read.table("HUMdurR_7520samples_96loci.map", sep="\t")
rs<- read.table("T2D_中日.txt", header=T)[,2]
data<- ped[, match(rs , map[,2])+6]
write.table(cbind(data[,1:6] , data), "T2D_中日_7520samples_31loci.ped", quote=F, col.names=F, row.names=F, sep="\t")

######################################################### training set & test set data QC
data<- read.table("HUMdurR_7520samples_96loci.ped" , sep="\t", colClass="character")
genotype<- apply(data[,7:dim(data)[2]] , 2 , function(x){
		x<- matrix(unlist(strsplit(x , split=" ")) , nc=2, byrow=T)
		x<- apply(x, 1, function(t) paste(sort(t) , collapse=""))
})

sample.call.rate<- apply(genotype , 1 , function(x) length(which(x!="NN"))/length(x) )
SNP.call.rate<- apply(genotype[which(sample.call.rate>=0.9),] , 2 , function(x) length(which(x!="NN"))/length(x) )
HWE.p.value<- apply(genotype[which(data[,6]==0 & sample.call.rate>=0.95),] , 2 , function(x){
			sample.allele<- unlist(strsplit(x[x!="NN"] , split=""))
			allele<- names(table(sample.allele))
			allele.freq<- as.numeric(table(sample.allele))/length(sample.allele)
			if(length(allele) == 1)
				return(1)
			else{
				expec.genotype.numb<- c(allele.freq[1]^2 , 2*allele.freq[1]*allele.freq[2] , allele.freq[2]^2) * length(x[x!="NN"])
				obser.genotype.numb<- as.numeric(table(factor(x[x!="NN"] , levels=paste(rep(allele , 2:1) , rep(allele , 1:2), sep=""))))
				thr<-sum((obser.genotype.numb - expec.genotype.numb)^2/expec.genotype.numb)
				return(1 - pchisq(thr , df=1))
			}
})

high.quality.data<- genotype[which(sample.call.rate>=0.9) , which(SNP.call.rate>=0.9 & HWE.p.value>=1e-4)]
code.number<- apply(high.quality.data[,2:dim(high.quality.data)[2]], 2 , function(x){
			is.na(x[x=="NN"]) <- T
			factor(x , label= 1:length(table(x))-1)
})
code.number<- cbind(data[which(sample.call.rate>=0.9) ,1] , code.number)
sample.IBS<- apply(code.number[1:6587,] , 1 , function(x){
			col.index<- which(code.number[,1] == x[1])
			apply(code.number[-c(1:col.index),] , 1 , function(y){
				t<- as.numeric(x[-1]) - as.numeric(y[-1])
				IBS<- sum(2 - abs(t) , na.rm= T) / length(t[!is.na(t)]) / 2
			})
})

png("sample_call_rate.png", height=600, width=800, units="px", pointsize=12)
par(c(5,6,4,2)+0.1)
hist(sample.call.rate, breaks=40, col="lightgrey", prob=T , cex.axis=2, xlab="", ylab="", main="")
	mtext(side=1, line=3, "Sample Call Rate", cex=2)
	mtext(side=2, line=2.5, "Density", cex=2)
dev.off()

png("SNP_CallRate_HWE.png", height=600, width=800, units="px", pointsize=12)
par(mfcol=c(2,1))
par(mar=c(0.5,6,2,2) + 0.1)
plot(NA, xlim=c(0, 30), ylim=c(0, 100), xaxt="n", cex.axis=2, xlab="", ylab="")
	mtext(side=2, line=3, "Call Rate %", cex=2)
	points(0:30, 100*SNP.call.rate, pch=4, cex=2, col=2)
par(mar=c(6.3,6,0.2,2) + 0.1)	
plot(NA, xlim=c(0, 30), ylim=c(0, 15), xaxt="n", cex.axis=2, xlab="", ylab="")
	mtext(side=2, line=3, text=expression(-log[10]("HWE p-value")) , cex=2)
	par(las=2)
	rs<- read.table("T2D_中日_7520samples_31loci.map")[,1]
	axis(1, 0:30, rs, cex=2, font=2)
	points(0:30, -log10(HWE.p.value), pch=4, cex=2, col=1)
dev.off()

png("sample_IBS.png", width = 800 , height = 600, units = "px", pointsize = 12)
par(mar=c(5,5,4,2)+0.1)
#data<- unlist(sample.IBS)
	hist(data, breaks=120, xlim=c(0.5,1), xlab="", ylab="", main="", probability=T, col="lightgrey", cex.axis=2)
		mtext("IBS Between Sample Pairs", side=1, line=3, cex=2, font=2)
		mtext("Frequency", side=2, line=3, cex=2, font=2)
par(fig=c(0.08, 0.5, 0.5, 0.98), new=T)
	hist(data[data>=0.92], breaks=40, xlim=c(0.92,1), xlab="", ylab="", main=list("plot with IBS>0.92", cex=1.5), probability=T, col=rainbow(40), cex.axis=1.5)
		mtext("IBS Between Sample Pairs", side=1, line=3, cex=1.5)
		mtext("Frequency", side=2, line=3, cex=1.5)
dev.off()
######################################################### pre-association based on our training data
data<- read.table("training_test_set.ped", sep="\t", colClass="character")
case<- which(data[,6]==1)
control<- which(data[,6]==0)
association<- apply(data[,-c(1:6)], 2, function(x){
			allele<- names(table(unlist(strsplit(x[x!="NN"] , split=""))))
			case.allele<- as.numeric(table(factor(unlist(strsplit(x[case] , split="")) , levels=allele)))
			control.allele<- as.numeric(table(factor(unlist(strsplit(x[control] , split="")) , levels=allele)))
			if(case.allele[1]/case.allele[2] >= control.allele[1]/control.allele[2]){
					contigency.table<- matrix(c(case.allele , control.allele), nc=2, byrow=T)
					association.test<- fisher.test(contigency.table , alternative = "two.sided")
					c(allele, case.allele , control.allele, association.test[[1]], association.test[[3]])
			}
			else{
					contigency.table<- matrix(c(case.allele[2], case.allele[1], control.allele[2], control.allele[1]), nc=2, byrow=T)
					association.test<- fisher.test(contigency.table , alternative = "two.sided")
					c(rev(allele), rev(case.allele) , rev(control.allele), association.test[[1]], association.test[[3]])
			}
})
rs<- read.table("training_test_set.map", header=T, sep="\t", colClass="character")[,2]
write.table(cbind(rs , t(association)), "association_test.txt", quote=F, col.names=F, row.names=F, sep="\t")

data<- read.table("association_test.txt", sep="\t")
png("association_test.png", height=600, width=800, units="px", pointsize=12)
par(mfcol=c(2,1))
par(mar=c(1,6,2,2) + 0.1)
plot(NA, xlim=c(0, 25), ylim=c(0, 1.5), xaxt="n", cex.axis=2, xlab="", ylab="")
	abline(h=seq(0, 1.5, by=0.3), col = "lightgray", lty = "dotted",)
	mtext(side=2, line=3, "Odds Ratio", cex=2)
	points(0:25, data[,9], pch=20, cex=2, col=c(rep(1:4, 6), 1:2))
par(mar=c(6,6,0,2) + 0.1)	
plot(NA, xlim=c(0, 25), ylim=c(0, 10), xaxt="n", cex.axis=2, xlab="", ylab="")
	mtext(side=2, line=3, text=expression(-log[10]("p-value")) , cex=2)
	abline(h=seq(0,10,by=2), col = "lightgray", lty = "dotted")
	par(las=2)
	points(0:25, -log10(data[,8]), pch=20, cex=2, col=c(rep(1:4, 6), 1:2))
	rs<- read.table("training_test_set.map", sep="\t", skip=1, colClass="character")[,2]
	#axis(side=1, at=0:25, rs, cex=2, font=2, col.ticks= c(rep(1:4, 6), 1:2))
	for(i in 1:26)
			mtext(side=1, line=0.5, at=i-1, rs[i], col= ifelse(i%%4==0, 4, i%%4), cex=1.2, font=2, srt=60)
dev.off()

#========================================================|
#		risk-based model				  |
#========================================================|
data<- read.table("training_test_set.ped", sep="\t", colClass="character")
risk.protect.allele<- read.table("association_test.txt", sep="\t", colClass="character")[,2:3]
risk.or<- read.table("association_test.txt", sep="\t", skip=0)[,9]
code.number<- data.frame(data[,1:6])
for(i in 1:26)
{
	x<- data[,i+6]
	is.na(x[x=="NN"])<- T
	if(sort(risk.protect.allele[i,])[1] == risk.protect.allele[i,1])
		x<- factor(x , levels=paste(rep(risk.protect.allele[i,], 2:1), rep(risk.protect.allele[i,], 1:2), sep="") , labels=2:0)
	else
		x<- factor(x , levels=paste(rep(rev(risk.protect.allele[i,]), 2:1), rep(rev(risk.protect.allele[i,]), 1:2), sep="") , labels=0:2)
	x<- as.numeric(as.character(x))
	x[is.na(x)]<- 0
	code.number<- cbind(code.number , x)
}

sample.risk<- apply(code.number[, -c(1:6)] , 1, function(x){
			risk<- 1
			for(i in 1:length(x))
				risk<- risk * risk.or[i]^x[i]
			risk
})
case<- sample.risk[which(data[,6]==1)]
control<- sample.risk[which(data[,6]==0)]
case.control<- cbind(c(case, control), c(rep(1, length(case)) , rep(0, length(control))))
anova( lm(case.control[,2] ~ case.control[,1]))

ROC<- NULL
for(risk.cutoff in sort(c(case , control)))
{
	positive<- length(case[case >= risk.cutoff])/length(case)
	false.positive<- length(control[control >= risk.cutoff])/length(control)
	ROC<- rbind(ROC , c(risk.cutoff, positive , false.positive))
}
AUC<- 0
for(i in 2:dim(ROC)[1])
{
	AUC<- AUC + (ROC[i,2] + ROC[i-1,2])*(ROC[i-1,3] - ROC[i,3])/2
}

predict<- NULL
for(risk.cutoff in sort(c(case , control)))
{
	positive.risk<- which(sample.risk >= risk.cutoff)
	negative.risk<- which(sample.risk < risk.cutoff)
	PPV<- length(which(data[positive.risk, 6] == 1))/length(positive.risk)
	NPV<- length(which(data[negative.risk, 6] == 0))/length(negative.risk)
	predict<- rbind(predict , c(risk.cutoff, PPV , NPV))
}

png("case_control_risk.png", height=600, width=800, units="px", pointsize=12)
par(mfcol=c(1,2), mar=c(5,5,4,2))
boxplot(case.control[,1] ~ case.control[,2], xlab="", ylab="", xaxt="n", col=3:2, cex.axis=2)
	mtext(side=2, line=3, "Disease Risk", cex=2)
	axis(side=1, at=1:2, c("Control", "Case"), cex=2, font=2)
plot(density(case.control[which(case.control[,2]==1), 1]), col=2, lwd=2, ylim=c(0, 0.015), cex.axis=2, cex.lab=2, xlab="Disease Risk", ylab="Density", main="")
	lines(density(case.control[which(case.control[,2]==0), 1]), col=3, lwd=2)
	legend(250, 0.015, col=c(2, 3), lwd=2, c("Case", "Control"), cex=1)
dev.off()

png("ROC_curve.png", height=600, width=600, units="px", pointsize=12)
par(mar=c(5,5,4,2))
plot(NA, xlim=c(0, 1), ylim=c(0, 1), cex.axis=2, cex.lab=2, xlab="False positive rate", ylab="True positive rate(sensitivity)")
	segments(0, 0, 1, 1, lwd=1, lty="solid")
	lines(ROC[,2], ROC[,1], lwd=2, col=2)
dev.off()

png("predict_value.png", height=600, width=600, units="px", pointsize=12)
par(mar=c(5,5,4,2))
plot(NA, xlim=c(0.5, 1), ylim=c(0, 1), cex.axis=2, cex.lab=2, xlab="Positive predice value", ylab="Negative predict value")
	#segments(0, 0, 1, 1, lwd=1, lty="solid")
	points(predict[,2], predict[,3], pch=20, cex=1, col=3)
dev.off()
#=======================================================|
#		classification-based model			 |
#=======================================================|
data<- read.table("training_test_set.ped", sep="\t")
code.number<- apply( data[, -c(1:6)], 2 , function(x){
			is.na(x[x=="NN"]) <- T
			factor(x , label= 1:length(table(x))-1)
})
storage.mode(code.number)<- "integer"
PCA<-	prcomp(code.number, scale=T)

## artificial compute
correlation.matrix<- cor(code.number , use="complete.obs")
eigen.value<- eigen(correlation.matrix)$values
eigen.vector<- eigen(correlation.matrix)$vector
sample.PCA<- code.number %*% eigen.vector[,1:3]
pairs(code.number %*% eigen.vector[,1:3])

library(lattice)
png("case-control_PC_visualization.png", height=600, width=600, units="px", pointsize=12)
cloud(sample.PCA[which(data[,6]!=-9),3] ~ sample.PCA[which(data[,6]!=-9),1] * sample.PCA[which(data[,6]!=-9),2], 
		pch=20, cex=1, col=3 - as.numeric(data[which(data[,6] != -9) , 6]), xlab="PC1", ylab="PC2", zlab="PC3",
		border=F, main=list(c("Case-red", "Control-green"), col=2:3, cex=2))
dev.off()

####################################### fisher discriminant analysis -- MASS package
library(MASS)
fit <- lda(code.number[which(code.number[,6]!=-9), -c(1:6)], grouping=as.numeric(code.number[which(code.number[,6]!=-9), 6]), prior=c(0.5, 0.5))
fit.coe<- fit[[4]]
sample.fit<- apply(code.number[,-c(1:6)], 1, function(x) sum(x * fit.coe) )
case<- sample.fit[which(code.number[,6]==1)]
control<- sample.fit[which(code.number[,6]==0)]
case.control<- cbind(c(case, control), c(rep(1, length(case)) , rep(0, length(control))))

index<- which(data[,6] != -9)
ROC.bootstrap<- NULL
for(i in 1:200){
	bootstrap<- sample(index , length(index), replace=T)
	fit <- lda(code.number[bootstrap, -c(1:6)], grouping=as.numeric(code.number[bootstrap, 6]), prior=c(0.5, 0.5))
	fit.coe<- fit[[4]]
	sample.fit<- apply(code.number[bootstrap,-c(1:6)], 1, function(x) sum(x * fit.coe) )
	case<- sample.fit[which(data[bootstrap,6]==1)]
	control<- sample.fit[which(data[bootstrap,6]==0)]
	ROC<- NULL
	for(risk.cutoff in sort(c(case , control)))
	{
		positive<- length(case[case >= risk.cutoff])/length(case)
		false.positive<- length(control[control >= risk.cutoff])/length(control)
		ROC<- rbind(ROC , c(positive , false.positive))
	}
	ROC.bootstrap<- cbind(ROC.bootstrap , ROC)
	#AUC<- 0
	#for(i in 2:dim(ROC)[1])
	#{
	#	AUC<- AUC + (ROC[i,2] + ROC[i-1,2])*(ROC[i-1,3] - ROC[i,3])/2
	#}
	#AUC.bootstrap<- rbind(AUC.bootstrap , AUC)
}

case<- which(code.number[,6] == 1)
control<- which(code.number[,6] == 0)
case.group<- sample(1:6, length(case) , prob=rep(1, 6)/6, replace=T)
control.group<- sample(1:6, length(control) , prob=rep(1, 6)/6, replace=T)
ROC.cross.validation<- NULL
AUC.bootstrap<-NULL
for(i in 1:5){
	for(j in 2:6){
			case.test<- which(case.group == i | case.group == j)
			control.test<- which(control.group == i | control.group == j)
			case.train<- which(case.group != i & case.group != j)
			control.train<- which(control.group != i & control.group != j)
			fit <- lda(code.number[c(case[case.train] , control[control.train]), -c(1:6)], grouping=as.numeric(code.number[c(case[case.train] , control[control.train]), 6]), prior=c(0.5, 0.5))
			fit.coe<- fit[[4]]
			sample.fit<- apply(code.number[c(case[case.test] , control[control.test]),-c(1:6)], 1, function(x) sum(x * fit.coe) )
	case.fit<- sample.fit[1:length(case.test)]
	control.fit<- sample.fit[(length(case.test)+1) : length(sample.fit)]
	ROC<- NULL
	for(risk.cutoff in sort(c(case.fit , control.fit)))
	{
		positive<- length(case.fit[case.fit >= risk.cutoff])/length(case.fit)
		false.positive<- length(control.fit[control.fit >= risk.cutoff])/length(control.fit)
		ROC<- rbind(ROC , c(positive , false.positive))
	}
	#ROC.cross.validation<- cbind(ROC.cross.validation , ROC)
	AUC<- 0
	for(t in 2:dim(ROC)[1])
	{
		AUC<- AUC + (ROC[t,1] + ROC[t-1,1])*(ROC[t-1,2] - ROC[t,2])/2
	}
	AUC.bootstrap<- rbind(AUC.bootstrap , AUC)
	}
}

png("case_control_lda_fit.png", height=600, width=800, units="px", pointsize=12)
par(mfcol=c(1,2), mar=c(5,5,4,2))
boxplot(case.control[,1] ~ case.control[,2], xlab="", ylab="", xaxt="n", col=3:2, cex.axis=2)
	mtext(side=2, line=3, "Disease Risk", cex=2)
	axis(side=1, at=1:2, c("Control", "Case"), cex=2, font=2)
plot(density(case.control[which(case.control[,2]==1), 1]), col=2, lwd=2, ylim=c(0, 0.5), cex.axis=2, cex.lab=2, xlab="Disease Risk", ylab="Density", main="")
	lines(density(case.control[which(case.control[,2]==0), 1]), col=3, lwd=2)
	legend(8, 0.5, col=c(2, 3), lwd=2, c("Case", "Control"), cex=1)
dev.off()

png("ROC_curve_lda.png", height=600, width=600, units="px", pointsize=12)
par(mar=c(5,5,4,2))
plot(NA, xlim=c(0, 1), ylim=c(0, 1), cex.axis=2, cex.lab=2, xlab="False positive rate", ylab="True positive rate(sensitivity)")
	segments(0, 0, 1, 1, lwd=1, lty="solid")
	lines(ROC[,3], ROC[,2], lwd=2, col=2)
dev.off()

png("ROC_AUC_lda_bootstrap.png", height=600, width=1200, units="px", pointsize=12)
par(mfcol=c(1, 2))
par(mar=c(5,5,4,2))
plot(NA, xlim=c(0, 1), ylim=c(0, 1), cex.axis=2, cex.lab=2, xlab="False positive rate", ylab="True positive rate(sensitivity)")
	segments(0, 0, 1, 1, lwd=1, lty="solid")
	for(i in 1:200){
		lines(ROC.bootstrap[,2*i], ROC.bootstrap[,2*i-1], lwd=1.5, col=rgb(0, 0, 0, 0.05))
	}
boxplot(AUC.bootstrap, xlab="", ylab="AUC", xaxt="n", col="light grey", cex.axis=2, cex.lab=2)
dev.off()
####################################### bayesian network