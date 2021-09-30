
plot_Figure_S_PCA_loadings <- function(origin){
  
  # packages
  library(vegan)
  library(ks) 
  library(stats) 
  library(calibrate)

  #-----------------
  # load data
  #-----------------
  load(file.path(origin,"data","master_matrix","X1.RData"))   # load PCA
  require(FactoMineR)
  X1_trait <- TRY_Env1[,Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_pred",replacement = ""))[,2]=="trait"]
  colnames(X1_trait) <- gsub(colnames(X1_trait),pattern = "_pred",replacement = "")
  dim(X1_trait)
  PCA_X1 <- PCA(log(X1_trait),ncp = ncol(X1_trait))
  
  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_5"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_5"))}
  
  nms=colnames(X1_trait)
  
  target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                  "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")
  ix= match(target_order1,nms)
  
  rownames(PCA_X1$var$coord) <- Rename_Vars(rownames(PCA_X1$var$coord) )[,3]
  
  pdf(file=file.path(origin, "figures","Supplement_Fig_6","figure_S1_PCA_loadings.pdf"),height=180,width=30mm)
    par(mfrow=c(2,3),mar=c(10,2,2,2))
    barplot(PCA_X1$var$coord[ix,1],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC1",main="PC1",cex.main=2)
    barplot(PCA_X1$var$coord[ix,2],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC2",main="PC2",cex.main=2)
    barplot(PCA_X1$var$coord[ix,3],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC3",main="PC3",cex.main=2)
    barplot(PCA_X1$var$coord[ix,4],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC4",main="PC4",cex.main=2)
    barplot(PCA_X1$var$coord[ix,5],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC5",main="PC5",cex.main=2)
  dev.off()

  pdf(file=file.path(origin, "figures","Supplement_Fig_6","figure_S1_PCA_loadings_abs.pdf"),height=10,width=10)
  par(mfrow=c(2,3),mar=c(10,2,2,2))
  barplot(abs(PCA_X1$var$coord[ix,1]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC1",main="PC1",cex.main=2)
  barplot(abs(PCA_X1$var$coord[ix,2]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC2",main="PC2",cex.main=2)
  barplot(abs(PCA_X1$var$coord[ix,3]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC3",main="PC3",cex.main=2)
  barplot(abs(PCA_X1$var$coord[ix,4]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC4",main="PC4",cex.main=2)
  barplot(abs(PCA_X1$var$coord[ix,5]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC5",main="PC5",cex.main=2)
  dev.off()
  
  pdf(file=file.path(origin, "figures","Supplement_Fig_5","figure_S1_PCA_evplvar.pdf"),height=6,width=9)
  par(mfrow=c(1,2),mar=c(6,5,2,2))
  barplot(PCA_X1$eig[,2],las=2,col=soil_col,ylab="Percentage of variance",cex.lab=1.5)
  abline(h = 10,col="red")
  barplot(PCA_X1$eig[,3],las=2,col=climate_col,ylab="Cumulative percentage of variance",cex.lab=1.5)
  abline(h = 50,col="red")
  dev.off()
  
}
