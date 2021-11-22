
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


if(!file.exists(file.path(origin,"figures","Supplement_Fig_40"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_40"))}


  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  require(FactoMineR)
  
  # ---------------------------------------------------------------------------------------
  # load data
  # ---------------------------------------------------------------------------------------
#  list.files(file.path(origin,"data","helper_files","fig_1b"))
  load(file = file.path(origin,"data","master_matrix","X1.RData"))
  load(file = file.path(origin,"data","master_matrix","X2.RData"))
  #load(file = file.path(origin,"data","helper_files","fig_1b","PCA.RData"))
  TRY_Env_o <- TRY_Env1
  load(file.path(origin,"data","helper_files","Ecoregions_selected","Ecoregion_Agg2.RData"))
  
  # ---------------------------------------------------------------------------------------
  # cut to relevant Ecoregions:
  # ---------------------------------------------------------------------------------------
  
  pca_FMr <- PCA(log(TRY_Env$trait))
  
  ix1=TRY_Env_o$Group.2%in%Ecoregion_Agg2$ECO_NAME
  TRY_Env_er <- TRY_Env_o[ix1,]
  dim(TRY_Env_er)
  dim(pca_FMr$ind$coord)
  
  ix2=abs(TRY_Env_er$Lat)>=29&abs(TRY_Env_er$Lat)<=36
  TRY_Env=TRY_Env_er[ix2,]
  ix1er=TRY_Env_er$Group.2%in%Ecoregion_Agg2$ECO_NAME
  sum(ix1er&ix2)
  dim(pca_FMr$ind$coord)
  TRY_pca <- pca_FMr$ind$coord[ix1er&ix2,]
  dim(TRY_Env)
  dim(TRY_pca)
  
  pdf(file=file.path(origin,"figures","figure_S_DipBiome","Figure_S_DipBiome.pdf"),width = 10,height = 10)
    par(mfrow=c(5,2),mar=c(4,2,2,2))
    biomes=unique(TRY_Env$BIOME)
    biom=4
    n=0
    for(biom in biomes){
      n=n+1
      ix=TRY_Env$BIOME==biom
      boxplot(TRY_pca[ix,2],col = color_to_biomes(biom),main=paste(Rename_Vars(biom)[,3]),
              cex.lab=1.5,xlab="PC2",cex.main=1.5,horizontal = TRUE,
              ylim=c(-8,5))
      abline(v=median(TRY_pca[,2]),col="black",lwd=2,lty=2)
      boxplot(TRY_pca[ix,2],col = color_to_biomes(biom),add=TRUE,horizontal = TRUE)
      abline(v=median(TRY_pca[ix,2]),col="blue",lwd=2)
      text(x = -7,y = .75,labels = paste0("n = ",sum(!is.na(TRY_pca[ix,2]))),col="gray",cex=1.5)
    }
  dev.off()

  