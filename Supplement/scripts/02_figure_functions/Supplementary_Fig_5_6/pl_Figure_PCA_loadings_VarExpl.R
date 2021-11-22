
# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
# origin = # please add your local path here & comment the ones below.
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Supplement" # please add your local path here 
list.files(file.path(origin,"scripts/_master"))

# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
# packages
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))
library(vegan)
library(ks) 
library(stats) 
library(calibrate)
require(FactoMineR)

target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")


#-----------------
# load data
#-----------------
# load data of sppecies per ecoregion aggregation level
load(file.path(origin,"data","master_matrix","X1.RData"))

# select data for PCA: traits that are imputed.
  X1_trait <- TRY_Env1[,Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_pred",replacement = ""))[,2]=="trait"]
  colnames(X1_trait) <- gsub(colnames(X1_trait),pattern = "_pred",replacement = "")
  dim(X1_trait)
# do PCA
  PCA_X1 <- PCA(log(X1_trait),ncp = ncol(X1_trait))
# order accroding to target_order1  
  nms=colnames(X1_trait)
  ix= match(target_order1,nms)
  rownames(PCA_X1$var$coord) <- Rename_Vars(rownames(PCA_X1$var$coord) )[,3]

# create figure folder
  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_5"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_5"))}
  
# plot.
  pdf(file=file.path(origin, "figures","Supplement_Fig_5","figure_S1_PCA_evplvar.pdf"),height=5,width=15)
  par(mfrow=c(1,2),mar=c(6,5,2,2))
    barplot(PCA_X1$eig[,2],las=2,col=soil_col,ylab="Percentage of variance",cex.lab=2,cex.axis=1.5)
    abline(h = 10,col="red")
    barplot(PCA_X1$eig[,3],las=2,col=climate_col,ylab="Cumulative percentage of variance",cex.lab=1.5,cex.axis=1.5)
    abline(h = 50,col="red")
  dev.off()
  
  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_6"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_6"))}
  
  pdf(file=file.path(origin, "figures","Supplement_Fig_6","figure_S1_PCA_loadings_abs.pdf"),height=10,width=15)
  par(mfrow=c(2,3),mar=c(10,6,4,2))
    barplot(abs(PCA_X1$var$coord[ix,1]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loadings",main="PC1",cex.main=4,cex.lab=2.5,cex.axis=1.5)
    barplot(abs(PCA_X1$var$coord[ix,2]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loadings",main="PC2",cex.main=4,cex.lab=2.5,cex.axis=1.5)
    barplot(abs(PCA_X1$var$coord[ix,3]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loadings",main="PC3",cex.main=4,cex.lab=2.5,cex.axis=1.5)
    barplot(abs(PCA_X1$var$coord[ix,4]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loadings",main="PC4",cex.main=4,cex.lab=2.5,cex.axis=1.5)
    barplot(abs(PCA_X1$var$coord[ix,5]),col=color_to_traits(nms)[ix],ylim=c(0,1),las=2,ylab="Loadings",main="PC5",cex.main=4,cex.lab=2.5,cex.axis=1.5)
  dev.off()
  

