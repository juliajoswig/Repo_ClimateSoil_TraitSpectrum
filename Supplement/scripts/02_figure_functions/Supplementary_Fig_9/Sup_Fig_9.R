
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

  require("dplyr")
  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  require(FactoMineR)

  trait_names=c("LeArea","SSD","SLA","LeC","LeN","LeP","PlantHeight", "SeedMass","SeLen","LeNArea","LeNP",
                "Led15N","SenbU","LeFMass","ConduitDens" ,"DispULen","VesLen"   )
  
  output_term="woody"
  #output_term="non_woody"
  #----------------------------------------------------------------------
  # load & prepare trait data
  #----------------------------------------------------------------------
  list.files(file.path(origin,"data","helper_files","Ecoregions_selected"))
  load(file.path(origin,"data","helper_files","Ecoregions_selected","Ecoregion_Agg2.RData")) # species per ecoregion scale
  X1_tot <- as.data.frame(read.csv(file.path(origin,"data","master_matrix",paste0("X1_",output_term,".csv"))))
  X1_gf <- X1_tot[,grep(colnames(X1_tot),pattern = "_gf")]
  colnames(X1_gf) <- gsub(colnames(X1_gf),pattern = "_gf",replacement = "")
  head(X1_gf)
  X1_gfc <- X1_gf[,Rename_Vars(colnames(X1_gf))[,2]=="trait"]
  X1_log <- as.data.frame(log(X1_gfc))
  X1 <- X1_log
  
  #----------------------------------------------------------------------
  # do PCA
  #----------------------------------------------------------------------
  require(FactoMineR)
  # for species per ecoregion scale
  pca_traits <- PCA(X1_log,ncp = 2)
  
  #----------------------------------------------------------------------
  # for latitudinal bin scale
  # aggregate to latitudinal bins
  #----------------------------------------------------------------------
  colnames(X1_gf)
  X1_lat <- aggregate(pca_traits$ind$coord,by = list(round(abs(X1_gf$Lat),digits = 0)),FUN = median,na.rm=TRUE)
  head(X1_lat)
  dim(X1_lat)

  #----------------------------------------------------------------------
  # calculate the window size for PC1 and PC2
  #----------------------------------------------------------------------
  # For PC1
  sc1 = cbind(abs(X1_gf$Lat),pca_traits$ind$coord[,1])
  list.files(file.path(origin,"data","helper_files","fig_S9",output_termwoo))
  if(file.exists(file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC1.RData"))){
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC1.RData"))
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"est_PC1.RData"))
  }else{
    sc <- sc1
    H <- Hpi(x=sc) # optimal bandwidth estimation
    save(H,file=file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC1.RData"))
    est <- kde(x=sc, H=H, compute.cont=TRUE) # kernel density estimation
    save(est,file=file.path(origin,"data","helper_files","fig_S9",output_term,"est_PC1.RData"))
    print("kernel estimation done!")
  }
  # For PC2
  sc2 = cbind(abs(X1_gf$Lat),pca_traits$ind$coord[,2])
  if(file.exists(file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC2.RData"))){
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC2.RData"))
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"est_PC2.RData"))
  }else{
    sc <- sc2
    H <- Hpi(x=sc) # optimal bandwidth estimation
    save(H,file=file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC2.RData"))
    est <- kde(x=sc, H=H, compute.cont=TRUE) # kernel density estimation
    save(est,file=file.path(origin,"data","helper_files","fig_S9",output_term,"est_PC2.RData"))
    print("kernel estimation done!")
  }
  
# create figure folder  
  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_9"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_9"))}
  #----------------------------------------------------------------------------
  # plot the PC1 against absolute latitude
  #----------------------------------------------------------------------------
  { 
    pdf(file=file.path(origin,"figures","Supplement_Fig_9",paste0("Figure_",output_term,".pdf")),width = 10,height = 8)
    par(mfrow=c(1,1),mar=c(6,6,2,2))
    
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC1.RData"))
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"est_PC1.RData"))
    est1=est
    dat1 <- data.frame(sc1)
    names(dat1) <- c("lat","PC")
    
    lm1 <- lm(PC~lat,data = dat1)
    lm1_agg <- lm(Dim.1~Group.1,data = X1_lat)
    
    # set contour probabilities for drawing contour levels
    cl <- contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    
    de_fit.m <- data.frame(matrix(data=NA,nrow=17,ncol=4))
    de_fit.m[,1] <- pca_traits$var$coord[,1]
    de_fit.m[,2] <- pca_traits$var$coord[,2]
    
    
    rownames(de_fit.m) <- rownames(pca_traits$var$coord)
    colnames(de_fit.m) <- c("PC1","PC2","r2","Pr(>1)")
    # arrows
    act.place.x <- vector(mode="numeric",length=nrow(de_fit.m))
    act.place.y <- vector(mode="numeric",length=nrow(de_fit.m))
    de_fit.m <- cbind(de_fit.m,act.place.x,act.place.y)
    
    
    ylab_now=paste0("PCA axis 1")
    xlab_now="Absolute latitude"
    plot(est, cont=seq(1,100,by=2), 
         display="filled.contour2", add=FALSE, cex.axis=2,cex.lab=2.5,
         ylab=ylab_now,xaxt="n",
         xlab="",
         xlim=c(-2,75),
         ylim=c(-8,6.2)
    )
    ## add the tick
    axis(1, at = seq(from=0,to=85,by = 20), label = rep("",5) , tck = -0.02,cex.lab=3)
    ## add the labels
    axis(1, at = seq(from=0,to=85,by = 20), lwd = 0, cex.axis = 0.9,cex.axis=2,line=1)
    ## add the xlab
    axis(side = 1,at=35,
         labels = xlab_now,tick = FALSE,
         cex.axis=3,line = 3.5)
    
    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=2, add=TRUE, lwd=2,lty=2, col= "dimgray")
    plot(est,abs.cont=cl[2], labels=c(0.8),labcex=2, add=TRUE, lwd=1.5,lty=3, col= "dimgray")
    plot(est,abs.cont=cl[3], labels=c(0.95),labcex=2, add=TRUE, lwd=.5,lty = 3, col= "dimgray")
    
    points(X1_lat$Group.1,X1_lat$Dim.1,pch=16,cex=2,col="white")
    points(X1_lat$Group.1,X1_lat$Dim.1,pch=16,cex=1.5)
    
    abline(lm1_agg,col="black",lwd=3)
    
    smr_lm1 <- summary(lm1)
    r2 = expression('r'^2* "=")
    text(x = 60,y = 5,labels = r2,cex=2,col = "gray")
    text(x = 67,y = 5,labels = round(smr_lm1$adj.r.squared,digits = 2),cex=2,col = "gray")
    smr_lm1_agg <- summary(lm1_agg)
    text(x = 60,y = 4,labels = r2,cex=2)
    text(x = 67,y = 4,labels = round(smr_lm1_agg$adj.r.squared,digits = 2),cex=2)
    
    #----------------------------------------------------------------------------
    # plot the PC2 against absolute latitude
    #----------------------------------------------------------------------------
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"H_PC2.RData"))
    load(file.path(origin,"data","helper_files","fig_S9",output_term,"est_PC2.RData"))
    est2=est
    dat1 <- data.frame(sc2)
    names(dat1) <- c("lat","PC")
    
    lm1 <- lm(PC~lat,data = dat1)
    lm1_agg <- lm(Dim.2~Group.1,data = X1_lat)
    
    # set contour probabilities for drawing contour levels
    cl <- contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    
    de_fit.m <- data.frame(matrix(data=NA,nrow=17,ncol=4))
    de_fit.m[,1] <- pca_traits$var$coord[,1]
    de_fit.m[,2] <- pca_traits$var$coord[,2]
    
    rownames(de_fit.m) <- rownames(pca_traits$var$coord)
    colnames(de_fit.m) <- c("PC1","PC2","r2","Pr(>1)")
    # arrows
    act.place.x <- vector(mode="numeric",length=nrow(de_fit.m))
    act.place.y <- vector(mode="numeric",length=nrow(de_fit.m))
    de_fit.m <- cbind(de_fit.m,act.place.x,act.place.y)
    
    
    ylab_now=paste0("PCA axis 2")
    xlab_now="Absolute latitude"
    plot(est, cont=seq(1,100,by=2), 
         display="filled.contour2", add=FALSE, cex.axis=2,cex.lab=2.5,
         ylab=ylab_now,xaxt="n",
         xlab="",
         xlim=c(-2,75),
         ylim=c(-8,6.2)
    )
    ## add the tick
    axis(1, at = seq(from=0,to=85,by = 20), label = rep("",5) , tck = -0.02,cex.lab=3)
    ## add the labels
    axis(1, at = seq(from=0,to=85,by = 20), lwd = 0, cex.axis = 0.9,cex.axis=2,line=1)
    ## add the xlab
    axis(side = 1,at=35,
         labels = xlab_now,tick = FALSE,
         cex.axis=3,line = 3.5)
    
    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=2, add=TRUE, lwd=2,lty=2, col= "dimgray")
    plot(est,abs.cont=cl[2], labels=c(0.8),labcex=2, add=TRUE, lwd=1.5,lty=3, col= "dimgray")
    plot(est,abs.cont=cl[3], labels=c(0.95),labcex=2, add=TRUE, lwd=.5,lty = 3, col= "dimgray")
    
    points(X1_lat$Group.1,X1_lat$Dim.2,pch=16,cex=2,col="white")
    points(X1_lat$Group.1,X1_lat$Dim.2,pch=16,cex=1.5)
    
    abline(lm1_agg,col="black",lwd=3)
    
    smr_lm1 <- summary(lm1)
    r2 = expression('r'^2* "=")
    text(x = 60,y = 5,labels = r2,cex=2,col = "gray")
    text(x = 67,y = 5,labels = round(smr_lm1$adj.r.squared,digits = 2),cex=2,col = "gray")
    smr_lm1_agg <- summary(lm1_agg)
    text(x = 60,y = 4,labels = r2,cex=2)
    text(x = 67,y = 4,labels = round(smr_lm1_agg$adj.r.squared,digits = 2),cex=2)
    
    dev.off()
  }
