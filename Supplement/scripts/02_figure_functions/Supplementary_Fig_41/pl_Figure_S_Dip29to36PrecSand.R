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


if(!file.exists(file.path(origin,"figures","Supplement_Fig_41"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_41"))}


  
  # ------------
  # load data
  # ------------
  output_term=""
  load(file = file.path(origin,"data","master_matrix",paste0("TRY_Env2_",output_term,"_2020.RData")))
  load(file = file.path(origin,"data","master_matrix",paste0("X2",output_term,".RData")))
  
  info=TRY_Env$info
  trait=TRY_Env$trait
  if(output_term=="PCA"){trait <-TRY_Env$trait_pca}
  soil=TRY_Env$soil
  climate=TRY_Env$climate
  
  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  require(FactoMineR)
  
  # ---------------------------------------------------------------------------------------
  # load data
  # ---------------------------------------------------------------------------------------
  list.files(file.path(origin,"data","helper_files"))
  load(file = file.path(origin,"data","master_matrix","TRY_Env1_20191111.RData"))
  load(file = file.path(origin,"data","master_matrix",paste0("X1",output_term,".RData")))
  
  # cut to relevant Ecoregions:
  load(file.path(origin,"data","helper_files","Ecoregions_selected","Ecoregion_Agg2.RData"))
  TRY_Env1=TRY_Env1[TRY_Env1$Group.2%in%Ecoregion_Agg2$ECO_NAME&
                      #abs(TRY_Env1$Lat)>=20&abs(TRY_Env1$Lat)<=50
                      abs(TRY_Env1$Lat)>=29&abs(TRY_Env1$Lat)<=36
                    #abs(TRY_Env1$Lat)>=60
                    ,]
  
  dim(TRY_Env1)
  TRY_Env_o <- TRY_Env1
  
  TRY_Env <- TRY_Env_o[,-c(grep(names(TRY_Env_o),pattern = "_pred"),
                           grep(names(TRY_Env_o),pattern = "_obs"))]
  names(TRY_Env) <- gsub(names(TRY_Env),pattern = "_gf",replacement = "")
  
  # split into traits and environment (soil & climate)
  traits_1 <- TRY_Env[,Rename_Vars(names(TRY_Env))[,2]=="trait"]
  Lat <- TRY_Env[,names(TRY_Env)=="Lat"][,1]
  info <- TRY_Env[,c(which(names(TRY_Env)%in%c("Lat","BIOME","ECO_ID","ECO_NAME")),
                     which(Rename_Vars(names(TRY_Env))[,2]%in%c("soil","climate")))]
  Lat1deg = round(Lat,digits = 0)

  #------------------------------------------------------------
  # create function
  #------------------------------------------------------------
  new.median.fun <- function(x) {
    
    if (is.numeric(x)&sum(!is.na(x)!=0)) {
      return(median(x[!is.na(x)]))
    } else {
      return(unique(x))
    }
  }
  
  get.evergreen.percentage <- function(x) {
    
    if (is.numeric(x)&sum(!is.na(x)!=0)) {
      return(median(x[!is.na(x)]))
    } else {
      return(unique(x))
    }
  }
  #------------------------------------------------------------
  # aggregate to Ecoregions
  #------------------------------------------------------------
  traits_2 <- aggregate(x = cbind(traits_1,info),
            by = list(info$ECO_NAME), FUN = new.median.fun)
  traits_2
  #------------------------------------------------------------
  # merge to lats of 1degree
  #------------------------------------------------------------
  # PCA
  env=log(traits_1)
  pca_FMr = PCA(env,scale=TRUE)
  dim(traits_1)
  length(Lat)
  # aggregate to species MEDIAN per lat # A1_late
  PCA_lat = aggregate(x=pca_FMr$ind,
                      by = list(abs(Lat1deg)), 
                      FUN=new.median.fun)
  # PCA_ER
  env2=log(traits_2[,Rename_Vars(names(traits_2))[,2]=="trait"])
  pca_FMr2 = PCA(env2,scale=TRUE)
  # aggregate to species MEDIAN per lat # A1_late
  PCA_lat2 = aggregate(x=pca_FMr$ind,
                       by = list(abs(Lat1deg)), 
                       FUN=new.median.fun)

  
  #----------------------------------------------------------------------------
  # plot the PC1 against absolute latitude
  #----------------------------------------------------------------------------

  pdf(file=file.path(origin,"figures","Supplement_Fig_41","Figure_S_Dip29to36PrecSand.pdf"),width = 10,height = 10)
    par(mfrow=c(2,2),mar=c(6,6,2,2))
    
    gradient=TRY_Env1$PrecipitationofDriestMonth.nc
    x <- gradient
    dat <- data.frame(x = x,y = x + 1)
    sr=c(seq(1,0,length.out = 100),rep(0,10))
    sg=c(rep(0,0),seq(.0,1,length.out = 50),seq(1,.5,length.out = 50),rep(0,0))
    sb=c(rep(0,30),seq(0,.7,length.out = 70),seq(1,.5,length.out = 70))
    
    colz_input=NA
    for(i in 1:100){
      colz_input = c(colz_input,rgb(sr[i],sg[i],sb[i]))
    }
    colz_input <- colz_input[!is.na(colz_input)]
    rbPal <- colorRampPalette(colz_input)
    n = length(gradient)
    colz <- rbPal(n)[as.numeric(cut(dat$y,breaks = n))]
    
    plot(TRY_Env1$SNDPPT_M_sl1_1km_ll.tif,pca_FMr$ind$coord[,1],col=colz,ylim=c(-6,6),pch=16,ylab="PC1 species",xlab="Sand faction",
         cex.axis=2,cex.lab=2)
    plot(TRY_Env1$SNDPPT_M_sl1_1km_ll.tif,pca_FMr$ind$coord[,2],col=colz,ylim=c(-6,6),pch=16,ylab="PC2 species",xlab="Sand faction",
         cex.axis=2,cex.lab=2)
    
    #--------------------------------------------------
    gradient=traits_2$PrecipitationofDriestMonth.nc
    x <- gradient
    dat <- data.frame(x = x,y = x + 1)
    sr=c(seq(1,0,length.out = 100),rep(0,10))
    sg=c(rep(0,0),seq(.0,1,length.out = 50),seq(1,.5,length.out = 50),rep(0,0))
    sb=c(rep(0,30),seq(0,.7,length.out = 70),seq(1,.5,length.out = 70))
    
    colz_input=NA
    for(i in 1:100){
      colz_input = c(colz_input,rgb(sr[i],sg[i],sb[i]))
    }
    colz_input <- colz_input[!is.na(colz_input)]
    rbPal <- colorRampPalette(colz_input)
    n = length(gradient)
    colz <- rbPal(n)[as.numeric(cut(dat$y,breaks = n))]
    
    plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,pca_FMr2$ind$coord[,1],col=colz,pch=16,ylab="PC1 ecoregions",xlab="Sand faction",
         cex.axis=2,cex.lab=2)
    plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,pca_FMr2$ind$coord[,2],col=colz,pch=16,ylab="PC2 ecoregions",xlab="Sand faction",
         cex.axis=2,cex.lab=2)
  
  dev.off()

  length(pca_FMr2$ind$coord[,2])
  length(pca_FMr$ind$coord[,1])
}
