
plot_Figure_S_PCA_loadings <- function(origin){
  
  # packages
  library(vegan)
  library(ks) 
  library(stats) 
  library(calibrate)

  #-----------------
  # load data
  #-----------------
  load(file.path(origin,"data","helper_files","fig_1b","PCA.RData"))   # load PCA

  if(!dir.exists(file.path(origin,"figures","figure_S_PCA_loadings"))){
    dir.create(file.path(origin,"figures","figure_S_PCA_loadings"))}
  nms=rownames(pca_FMr$var$coord)
  rownames(pca_FMr$var$coord) <- Rename_Vars(rownames(pca_FMr$var$coord) )[,3]
  
  target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                  "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")
  ix= match(target_order1,nms)
  dim(pca_FMr$ind$coord)
  pdf(file=file.path(origin, "figures","figure_S_PCA_loadings","figure_S1_PCA_loadings.pdf"),height=10,width=10)
    par(mfrow=c(2,3),mar=c(10,2,2,2))
    barplot(pca_FMr$var$coord[ix,1],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC1",main="PC1",cex.main=2)
    barplot(pca_FMr$var$coord[ix,2],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC2",main="PC2",cex.main=2)
    barplot(pca_FMr$var$coord[ix,3],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC3",main="PC3",cex.main=2)
    barplot(pca_FMr$var$coord[ix,4],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC4",main="PC4",cex.main=2)
    barplot(pca_FMr$var$coord[ix,5],col=color_to_traits(nms)[ix],ylim=c(-1,1),las=2,ylab="Loading PC5",main="PC5",cex.main=2)
  dev.off()

  pdf(file=file.path(origin, "figures","figure_S_PCA_loadings","figure_S1_PCA_loadings_abs.pdf"),height=10,width=10)
  par(mfrow=c(2,3),mar=c(10,2,2,2))
  barplot(abs(pca_FMr$var$coord[ix,1]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC1",main="PC1",cex.main=2)
  barplot(abs(pca_FMr$var$coord[ix,2]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC2",main="PC2",cex.main=2)
  barplot(abs(pca_FMr$var$coord[ix,3]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC3",main="PC3",cex.main=2)
  barplot(abs(pca_FMr$var$coord[ix,4]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC4",main="PC4",cex.main=2)
  barplot(abs(pca_FMr$var$coord[ix,5]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loading PC5",main="PC5",cex.main=2)
  dev.off()
  
  pdf(file=file.path(origin, "figures","figure_S_PCA_loadings","figure_S1_PCA_evplvar.pdf"),height=6,width=9)
  par(mfrow=c(1,2),mar=c(6,5,2,2))
  barplot(pca_FMr$eig[,2],las=2,col=soil_col,ylab="Percentage of variance",cex.lab=1.5)
  abline(h = 10,col="red")
  barplot(pca_FMr$eig[,3],las=2,col=climate_col,ylab="Cumulative percentage of variance",cex.lab=1.5)
  abline(h = 50,col="red")
  dev.off()
  
}
