plot_Figure_x <- function(origin,output_term){
  
  

  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  require(FactoMineR)
  
  # ---------------------------------------------------------------------------------------
  # load data
  # ---------------------------------------------------------------------------------------
  list.files(file.path(origin,"data","helper_files","fig_1b"))
  load(file = file.path(origin,"data","master_matrix","TRY_Env1_20191111.RData"))
  load(file = file.path(origin,"data","helper_files","fig_1b","PCA.RData"))
  TRY_Env_o <- TRY_Env1
  load(file.path(origin,"data","helper_files","Ecoregions_selected","Ecoregion_Agg2.RData"))
  
  # ---------------------------------------------------------------------------------------
  # cut to relevant Ecoregions:
  # ---------------------------------------------------------------------------------------
  ix1=TRY_Env_o$Group.2%in%Ecoregion_Agg2$ECO_NAME
  TRY_Env_er <- TRY_Env_o[ix1,]
  dim(TRY_Env_er)
  dim(pca_FMr$ind$coord)
  
  ix2=abs(TRY_Env_er$Lat)>=29&abs(TRY_Env_er$Lat)<=36
  TRY_Env=TRY_Env_er[ix2,]
  ix1er=TRY_Env_er$Group.2%in%Ecoregion_Agg2$ECO_NAME
  TRY_pca <- pca_FMr$ind$coord[ix1er&ix2,]
  dim(TRY_Env)
  dim(TRY_pca)
  
  
  plot(log(TRY_Env$PlantHeight),log(TRY_Env$SLA),col=color_to_biomes(TRY_Env$BIOME))
  plot(log(TRY_Env_o2$PlantHeight_gf),log(TRY_Env_o2$SLA_gf),col=color_to_biomes(TRY_Env$BIOME))
  
  plot(log(TRY_Env$ConduitDens),log(TRY_Env$SLA),col=color_to_biomes(TRY_Env$BIOME))
  plot(log(TRY_Env_o2$ConduitDens_gf),log(TRY_Env_o2$SLA_gf),col=color_to_biomes(TRY_Env$BIOME))
  
  
  
  boxplot(TRY_Env)
  
  
  
  
  
  dat_lm <- data.frame(t1=log(TRY_Env$ConduitDens),t2=log(TRY_Env$SLA))
  lm_sub<- lm(formula = t1~t2,data = dat_lm)
  smr_sub <- summary(lm_sub)
  smr_sub$adj.r.squared
  dat_lm2 <- data.frame(t1=log(TRY_Env_o2$ConduitDens_gf),t2=log(TRY_Env_o2$SLA_gf))
  lm_tot<- lm(formula = t1~t2,data = dat_lm2)
  smr_tot <- summary(lm_tot)
  smr_tot$adj.r.squared
  
  # split into traits and environment (soil & climate)
  traits_1 <- TRY_Env[,c(which(Rename_Vars(names(TRY_Env))[,2]=="trait"),
                         which(names(TRY_Env)%in%c("Lat","BIOME","ECO_ID","ECO_NAME")),
                         which(Rename_Vars(names(TRY_Env))[,2]%in%c("soil","climate")))]
  Lat <- TRY_Env[,names(TRY_Env)=="Lat"][,1]
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
  
  #------------------------------------------------------------
  # aggregate to Ecoregions
  #------------------------------------------------------------
  traits_2 <- aggregate(x = cbind(traits_1),
            by = list(info$ECO_NAME), FUN = new.median.fun)
  traits_2
  #------------------------------------------------------------
  # merge to lats of 1degree
  #------------------------------------------------------------
  # PCA
  env = log(traits_1[,Rename_Vars(names(traits_1))[,2]=="trait"])
  pca_FMr = PCA(env,scale=TRUE)
  
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

  x <- traits_1$PrecipitationofDriestMonth.nc
    dat <- data.frame(x = x,y = x + 1)
    sr=c(seq(1,0,length.out = 100),rep(0,10))
    sg=c(rep(0,0),seq(.0,1,length.out = 50),seq(1,.5,length.out = 50),rep(0,0))
    sb=c(rep(0,30),seq(0,.7,length.out = 70),seq(1,.5,length.out = 70))
    colz_input=NA
    for(i in 1:100){
      colz_input = c(colz_input,rgb(sr[i],sg[i],sb[i]))
    }
    colz_input <- colz_input[!is.na(colz_input)]
    n = length(x)
    rbPal <- colorRampPalette(colz_input)
  colz1<- rbPal(n)[as.numeric(cut(dat$y,breaks = n))]
  
  x <- traits_2$PrecipitationofDriestMonth.nc
    dat <- data.frame(x = x,y = x + 1)
    sr=c(seq(1,0,length.out = 100),rep(0,10))
    sg=c(rep(0,0),seq(.0,1,length.out = 50),seq(1,.5,length.out = 50),rep(0,0))
    sb=c(rep(0,30),seq(0,.7,length.out = 70),seq(1,.5,length.out = 70))
    colz_input=NA
    for(i in 1:100){
      colz_input = c(colz_input,rgb(sr[i],sg[i],sb[i]))
    }
    colz_input <- colz_input[!is.na(colz_input)]
    n = length(x)
    rbPal <- colorRampPalette(colz_input)
  colz2 <- rbPal(n)[as.numeric(cut(dat$y,breaks = n))]
  
  
  pdf(file=file.path(origin,"figures","figure_S8","Figure_S8_Sand_29to36_PCA.pdf"),width = 15,height = 10)
    par(mfrow=c(2,2),mar=c(6,6,2,2))
    plot(traits_1$SNDPPT_M_sl1_1km_ll.tif,pca_FMr$ind$coord[,1],col=add.alpha(colz1,.5),pch=16,ylab="PC1",xlab="Sand fraction",cex.lab=2)
    plot(traits_1$SNDPPT_M_sl1_1km_ll.tif,pca_FMr$ind$coord[,2],col=add.alpha(colz1,.5),pch=16,ylab="PC2",xlab="Sand fraction",cex.lab=2)
    
    plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,pca_FMr2$ind$coord[,1],col=colz2,pch=16,ylab="PC1",xlab="Sand fraction",cex=2,cex.lab=2)
    plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,pca_FMr2$ind$coord[,2],col=colz2,pch=16,ylab="PC2",xlab="Sand fraction",cex=2,cex.lab=2)
  dev.off()
  
  
  
  pdf(file=file.path(origin,"figures","figure_S8","Figure_S8_latgradient29to36_EcoTraits.pdf"),width = 20,height = 8)
    par(mfrow=c(2,4),mar=c(6,6,2,2))
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$SLA),col=colz2,pch=16,ylab="SLA",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$LeN),col=colz2,pch=16,ylab="Leaf N",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$LeP),col=colz2,pch=16,ylab="Leaf P",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$LeNP),col=colz2,pch=16,ylab="Leaf N:P ratio",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$LeC),col=colz2,pch=16,ylab="Leaf C",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$LeNArea),col=colz2,pch=16,ylab="Leaf N per leaf area",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$SSD),col=colz2,pch=16,ylab="Stem specific density (SSD)",xlab="Sand fraction",cex=2,cex.lab=2)
  dev.off()
  
  pdf(file=file.path(origin,"figures","figure_S8","Figure_S8_latgradient29to36_SizeTraits.pdf"),width = 20,height = 8)
    par(mfrow=c(2,4),mar=c(6,6,2,2))
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$ConduitDens),col=colz2,pch=16,ylab="ConduitDens",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$LeArea),col=colz2,pch=16,ylab="Leaf area",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$PlantHeight),col=colz2,pch=16,ylab="Plant height",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$SeedMass),col=colz2,pch=16,ylab="Seed mass",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$SeLen),col=colz2,pch=16,ylab="Seed legth",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$DispULen),col=colz2,pch=16,ylab="Dispersal U length",xlab="Sand fraction",cex=2,cex.lab=2)
      plot(traits_2$SNDPPT_M_sl1_1km_ll.tif,log(traits_2$LeFMass),col=colz2,pch=16,ylab="Leaf fresh mass",xlab="Sand fraction",cex=2,cex.lab=2)
  dev.off()
  
  #V2 Precipitation
  #--------------------------------------------------------------------------------------------------------
  x <- traits_1$SNDPPT_M_sl1_1km_ll.tif
    dat <- data.frame(x = x,y = x + 1)
    sr=c(seq(1,0,length.out = 100),rep(0,10))
    sg=c(rep(0,0),seq(.0,1,length.out = 50),seq(1,.5,length.out = 50),rep(0,0))
    sb=c(rep(0,30),seq(0,.7,length.out = 70),seq(1,.5,length.out = 70))
    colz_input=NA
    for(i in 100:1){
      colz_input = c(colz_input,rgb(sr[i],sg[i],sb[i]))
    }
    colz_input <- colz_input[!is.na(colz_input)]
    n = length(x)
    rbPal <- colorRampPalette(colz_input)
  colz1 <- rbPal(n)[as.numeric(cut(dat$y,breaks = n))]
  
  
  x <- traits_2$SNDPPT_M_sl1_1km_ll.tif
    dat <- data.frame(x = x,y = x + 1)
    sr=c(seq(1,0,length.out = 100),rep(0,10))
    sg=c(rep(0,0),seq(.0,1,length.out = 50),seq(1,.5,length.out = 50),rep(0,0))
    sb=c(rep(0,30),seq(0,.7,length.out = 70),seq(1,.5,length.out = 70))
    colz_input=NA
    for(i in 100:1){
      colz_input = c(colz_input,rgb(sr[i],sg[i],sb[i]))
    }
    colz_input <- colz_input[!is.na(colz_input)]
    n = length(x)
    rbPal <- colorRampPalette(colz_input)
  colz2 <- rbPal(n)[as.numeric(cut(dat$y,breaks = n))]
  
  
  
  pdf(file=file.path(origin,"figures","figure_S_DipBiome","Figure_S8_Precip_29to36_PCA.pdf"),width = 15,height = 5)
  par(mfrow=c(1,2),mar=c(6,6,2,2))
    plot(traits_1$PrecipitationofDriestMonth.nc,pca_FMr$ind$coord[,1],col=add.alpha(colz1,.5),pch=16,ylab="PC1",xlab="Precipitation of dryest month")
    plot(traits_1$PrecipitationofDriestMonth.nc,pca_FMr$ind$coord[,2],col=add.alpha(colz1,.5),pch=16,ylab="PC2",xlab="Precipitation of dryest month")
    
    plot(traits_2$PrecipitationofDriestMonth.nc,pca_FMr2$ind$coord[,1],col=colz2,pch=16,ylab="PC1",xlab="Precipitation of dryest month",cex=2)
    plot(traits_2$PrecipitationofDriestMonth.nc,pca_FMr2$ind$coord[,2],col=colz2,pch=16,ylab="PC2",xlab="Precipitation of dryest month",cex=2)
  dev.off()
  
  
  
  
  pdf(file=file.path(origin,"figures","Figure_X_latgradient29to36_EcoTraits_V2.pdf"),width = 20,height = 8)
  par(mfrow=c(2,4),mar=c(6,6,2,2))
    plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$SLA),col=colz2,pch=16,ylab="SLA",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
    plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$LeN),col=colz2,pch=16,ylab="Leaf N",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
    plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$LeP),col=colz2,pch=16,ylab="Leaf P",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
    plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$LeNP),col=colz2,pch=16,ylab="Leaf P",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
    plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$LeC),col=colz2,pch=16,ylab="Leaf C",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
    plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$LeNArea),col=colz2,pch=16,ylab="Leaf N per leaf area",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
    plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$SSD),col=colz2,pch=16,ylab="Stem specific density (SSD)",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  dev.off()
  
  pdf(file=file.path(origin,"figures","Figure_X_latgradient29to36_SizeTraits_V2.pdf"),width = 20,height = 8)
  
  par(mfrow=c(2,4),mar=c(6,6,2,2))
  plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$ConduitDens),col=colz2,pch=16,ylab="ConduitDens",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$LeArea),col=colz2,pch=16,ylab="Leaf area",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$PlantHeight),col=colz2,pch=16,ylab="Plant height",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$SeedMass),col=colz2,pch=16,ylab="Seed mass",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$SeLen),col=colz2,pch=16,ylab="Seed legth",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$DispULen),col=colz2,pch=16,ylab="Dispersal U length",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  plot(traits_2$PrecipitationofDriestMonth.nc,log(traits_2$LeFMass),col=colz2,pch=16,ylab="Leaf fresh mass",xlab="Precipitation of dryest month",cex=2,cex.lab=2)
  dev.off()
  
}
