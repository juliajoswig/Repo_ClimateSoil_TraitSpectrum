setwd("/..")
origin = "Volumes/bgi/work_1/2016_GapFilling"
#origin="2016_GapFilling"
res_matrix_name="res_20201020_cor"
#res_o=res
res <- read.table(file.path(origin,"runs","META",paste0(res_matrix_name,".csv")),sep=",",dec=".")
summary(res)
list.files(file.path(origin,"runs","META"))
#colz=rainbow(7)#c("red","blue","green","magenta","turquoise","orange","yellow")#
#colz=c("#d73027","#f46d43","#fdae61","#fee08b","#d9ef8b","#a6d96a","#66bd63","#1a9850")
#colz=c("#d7191c","#2c7bb6","#fdae61","#abd9e9")#"#ffffbf",
#colz1=c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")#"#ffffbf",
#colz2=c("#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000")#"#ffffbf",
output_term="2020"

# Cor changes with gap-size all traits
# Corrs for traits

#--------------------------------------
# RMSE increases with gap-size
#--------------------------------------
sel_ObSpe1="Obs_obs_TD"
pattern_now="cor_"
#chose guido
ix1=res[,colnames(res)=="TraitChoice"]=="guido"&res[,colnames(res)=="Obs_or_Spec"]==sel_ObSpe1
Percent1 <- res[ix1,colnames(res)=="GapPercent"]
mode(Percent1) <- "numeric"

dat_rmse <- res[ix1,grep(colnames(res),pattern = pattern_now)]
colnames(dat_rmse)
dat_rmse <- as.matrix(dat_rmse)
mode(dat_rmse)="numeric"
dat_rmse <- dat_rmse[,colSums(!is.na(dat_rmse))!=0]
colnames(dat_rmse)
dat_plot<- cbind(Percent1,dat_rmse)
dat_plot <- dat_plot[complete.cases(dat_plot),]
dat_orig <- dat_plot[dat_plot[,1]==-1,]
dat_plotf <- dat_plot[dat_plot[,1]!=-1,]

for(i in 2:ncol(dat_plot)){
  dat_plotf[,i] <- dat_plotf[,i] -  dat_orig[,i][1]
}

dat_plotf_mean <- cbind(dat_plotf[,1],rowMeans(dat_plotf[,2:ncol(dat_plotf)],na.rm = TRUE))



#chose rainfor
ix2=res[,colnames(res)=="TraitChoice"]=="rainfor"&res[,colnames(res)=="Obs_or_Spec"]==sel_ObSpe1
Percent1 <- res[ix1,colnames(res)=="GapPercent"]
mode(Percent1) <- "numeric"

dat_rmse2 <- res[ix2,grep(colnames(res),pattern = pattern_now)]
colnames(dat_rmse2)
dat_rmse2 <- as.matrix(dat_rmse2)
mode(dat_rmse2)="numeric"
dat_rmse2 <- dat_rmse2[,colSums(!is.na(dat_rmse2))!=0]
colnames(dat_rmse2)
dat_plot2<- cbind(Percent2,dat_rmse2)
dat_plot2 <- dat_plot2[complete.cases(dat_plot2),]
dat_orig2 <- dat_plot2[dat_plot2[,1]==-1,]
dat_plotf2 <- dat_plot2[dat_plot2[,1]!=-1,]

for(i in 2:ncol(dat_plot2)){
  dat_plotf2[,i] <- dat_plotf2[,i] -  dat_orig2[,i][1]
}

dat_plotf2_mean <- cbind(dat_plotf2[,1],rowMeans(dat_plotf2[,2:ncol(dat_plotf2)],na.rm = TRUE))

pdf(file=file.path(origin,"plots","Correl","Cor_increases_Gap_size_dist.pdf"),width=8,height=5)
  par(mfrow=c(1,2),mar=c(4,4,2,2))
  plot(dat_plotf_mean,col=colz1[8],pch=16,ylim=c(-.5,.5),main="Correlations data set 1 (abs., mean)",xlab="% Gaps",ylab="Pearson correlation coefficient",xlim=c(0,72))
  abline(h=seq(-1,1.2,by=.2),col="gray",lty=2)
  abline(h=0,col="black")
  plot(dat_plotf2_mean,col=colz2[8],pch=16,ylim=c(-.5,.5),main="Correlations data set 2 (abs., mean)",xlab="% Gaps",ylab="Pearson correlation coefficient",xlim=c(0,72))
  abline(h=seq(0,1.2,by=.2),col="gray",lty=2)
  abline(h=0,col="black")
dev.off()



pdf(file=file.path(origin,"plots","Correl","Cor_increases_Gap_size.pdf"),width=8,height=5)
par(mfrow=c(1,2),mar=c(4,4,2,2))
plot(dat_plot,col=colz1[8],pch=16,ylim=c(0,1),main="Correlations data set 1 (abs., mean)",xlab="% Gaps",ylab="Pearson correlation coefficient",xlim=c(0,72))
abline(h=seq(0,1.2,by=.2),col="gray",lty=2)
plot(dat_plot2,col=colz2[8],pch=16,ylim=c(0,1),main="Correlations data set 2 (abs., mean)",xlab="% Gaps",ylab="Pearson correlation coefficient",xlim=c(0,72))
abline(h=seq(0,1.2,by=.2),col="gray",lty=2)
dev.off()


#--------------------------------------
# RMSE different for traits
#--------------------------------------
pdf(file=file.path(origin,"plots","Correl","Cor_per_trait.pdf"),width=8,height=5)
par(mfrow=c(1,2),mar=c(4,4,2,2))
i=1
dat_ploti <- cbind(Percent1,dat_rmse[,i])
dat_ploti <- dat_ploti[complete.cases(dat_ploti),]
dat_plotim<- aggregate(dat_ploti[,2],by=list(dat_ploti[,1]),FUN=mean)
plot(dat_plotim,col=colz1[8],pch=16,ylim=c(-1,1),main="Correlations data set 1 (abs., mean)",xlab="% Gaps",ylab="Pearson correlation coefficient",xlim=c(0,72))
abline(h=seq(-1,1.2,by=.2),col="gray",lty=2)
lines(dat_plotim[order(dat_plotim[,1]),],col=colz1[8])
i=1
for(i in 1:ncol(dat_rmse)){
  dat_ploti <- cbind(Percent1,dat_rmse[,i])
  dat_ploti <- dat_ploti[complete.cases(dat_ploti),]
  dat_plotim<- aggregate(dat_ploti[,2],by=list(dat_ploti[,1]),FUN=mean)
  points(dat_plotim,col=colz1[8],pch=16)
  lines(dat_plotim[order(dat_plotim[,1]),],col=colz1[8])
  print(mean(dat_plotim[,2]))
}
dat_ploti <- cbind(Percent2,dat_rmse2[,i])
dat_ploti <- dat_ploti[complete.cases(dat_ploti),]
dat_plotim<- aggregate(dat_ploti[,2],by=list(dat_ploti[,1]),FUN=mean)
plot(dat_plotim,col=colz2[8],pch=16,ylim=c(-1,1),main="Correlations data set 2 (abs., mean)",xlab="% Gaps",ylab="Pearson correlation coefficient",xlim=c(0,72))
abline(h=seq(-1,1.2,by=.2),col="gray",lty=2)
lines(dat_plotim[order(dat_plotim[,1]),],col=colz2[8])
i=1
for(i in 1:ncol(dat_rmse)){
  dat_ploti <- cbind(Percent2,dat_rmse2[,i])
  dat_ploti <- dat_ploti[complete.cases(dat_ploti),]
  dat_plotim<- aggregate(dat_ploti[,2],by=list(dat_ploti[,1]),FUN=mean)
  points(dat_plotim,col=colz2[8],pch=16)
  lines(dat_plotim[order(dat_plotim[,1]),],col=colz2[8])
  print(mean(dat_plotim[,2]))
}

dev.off()
