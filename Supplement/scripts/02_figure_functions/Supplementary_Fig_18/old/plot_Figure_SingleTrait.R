plot_Figure_2 <- function(origin){
  
  
  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  
  #------------------------------------------------------------
  # load data
  #------------------------------------------------------------
  load("/Volumes/bgi/people/ssippel/projects/TRAIT_ENVIRONMENT/v1_20181003/data/_aggregated_agg1/TRY_DCS1_WorldClim.RData")
    data_agg1_1 <- TRY_DCS1_WorldClim
    trait_agg1_2 <- data_agg1_1[,put_into_TraitsOrClimateOrSoil(colnames(data_agg1_1))=="trait"]
    soil_agg1_2 <- data_agg1_1[,put_into_TraitsOrClimateOrSoil(colnames(data_agg1_1))=="soil"]
    climate_agg1_2 <- data_agg1_1[,put_into_TraitsOrClimateOrSoil(colnames(data_agg1_1))=="climate"]
    lat_agg1_2 <- data_agg1_1[,colnames(data_agg1_1)=="lat"]
  
  # binning needed
  lat_bins <- round(lat_agg1_2,0)
  data_trait <-cbind(lat_bins,trait_agg1_2) 
  data_trait <- as.data.frame(data_trait)
  
  new.median.fun  <- function(x) {
    if (is.numeric(x)) {
      return(median(x, na.rm=T))
    } else {
      return(unique(x))
    }
  }
  
  data_trait_ag <- aggregate(x=data_trait[which(!(names(data_trait) %in% c("lat_bins")))], 
                             by = list(data_trait$lat_bins), FUN=new.median.fun)
  data_trait_o <- data_pca_ag[order(data_trait_ag$Group.1),]
  
  
  

    
    #----------------------------------------------------------------------------
    # plot the PC1 against absolute latitude
    #----------------------------------------------------------------------------
    
    pdf(file=file.path(origin,"plots","Fig_2","Fig2_test.pdf"),width = 20,height = 8)
    par(mfrow=c(1,2),mar=c(4,6,2,2))
    
      load(file.path(origin,"data","Helper_files","H_PC1.RData"))
      load(file.path(origin,"data","Helper_files","est_PC1.RData"))
      est1=est
      sc <-cbind(abs(Trait_data$lat),pca_FMr$ind$coord[,1]) #scores(env.pca, choices=c(1,2), display=c("species"))  
      sc1=sc
      dat1 <- data.frame(sc1)
      names(dat1) <- c("lat","PC")
      
      lm1 <- lm(PC~lat,data = dat1)
      lm1_agg <- lm(coord.Dim.1~Group.1,data = PCA_lat)
      
      # set contour probabilities for drawing contour levels
      cl <- contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
      
      de_fit.m <- data.frame(matrix(data=NA,nrow=length(trait.names),ncol=4))
      de_fit.m[,1] <- pca_FMr$var$coord[,1]
      de_fit.m[,2] <- pca_FMr$var$coord[,2]
      
      
      rownames(de_fit.m) <- rownames(pca_FMr$var$coord)
      colnames(de_fit.m) <- c("PC1","PC2","r2","Pr(>1)")
      # arrows
      act.place.x <- vector(mode="numeric",length=nrow(de_fit.m))
      act.place.y <- vector(mode="numeric",length=nrow(de_fit.m))
      de_fit.m <- cbind(de_fit.m,act.place.x,act.place.y)
      
      
      ylab_now=expression("PC"[1])
      xlab_now="Absolute latitude"
      plot(est, cont=seq(1,100,by=2), display="filled.contour2", add=FALSE, cex.axis=2,cex.lab=2,
           ylab=ylab_now,
           xlab=xlab_now,
           ylim=c(-8,6)
      )
      
      
      plot(est,abs.cont=cl[1], labels=c(0.5),labcex=1, add=TRUE, lwd=2,lty=2, col= "dimgray")
      plot(est,abs.cont=cl[2], labels=c(0.8),labcex=1, add=TRUE, lwd=1.5,lty=3, col= "dimgray")
      plot(est,abs.cont=cl[3], labels=c(0.95),labcex=1, add=TRUE, lwd=.5,lty = 3, col= "dimgray")
      
      points(PCA_lat$Group.1,PCA_lat$coord.Dim.1,pch=16,cex=2,col="white")
      points(PCA_lat$Group.1,PCA_lat$coord.Dim.1,pch=16,cex=1.5)
      
      abline(lm1_agg,col="black",lwd=3)
      
      smr_lm1 <- summary(lm1)
      r2 = expression('r'^2* "=")
      text(x = 70,y = 5,labels = r2,cex=2,col = "gray")
      text(x = 77,y = 5,labels = round(smr_lm1$adj.r.squared,digits = 2),cex=2,col = "gray")
      smr_lm1_agg <- summary(lm1_agg)
      text(x = 70,y = 4,labels = r2,cex=2)
      text(x = 77,y = 4,labels = round(smr_lm1_agg$adj.r.squared,digits = 2),cex=2)
      
    #----------------------------------------------------------------------------
    # plot the PC2 against absolute latitude
    #----------------------------------------------------------------------------
      load(file.path(origin,"data","Helper_files","H_PC2.RData"))
      load(file.path(origin,"data","Helper_files","est_PC2.RData"))
    
      est2=est
      sc <-cbind(abs(Trait_data$lat),pca_FMr$ind$coord[,2])
      sc2 <- sc
      dat2 <- data.frame(sc2)
      names(dat2) <- c("lat","PC")
      
      lm1 <- lm(PC~lat,data = dat2)
      lm1_agg <- lm(coord.Dim.2~Group.1,data = PCA_lat)
      
      sc2 <-cbind(abs(Trait_data$lat),pca_FMr$ind$coord[,2]) #scores(env.pca, choices=c(1,2), display=c("species"))  
      dat2 <- data.frame(sc2)
      names(dat2) <- c("lat","PC")
      lm2 <- lm(PC~lat,data = dat2)
      lm2_agg <- lm(coord.Dim.2~Group.1,data = PCA_lat)
      
      sc=sc2
      est=est2
      
      # set contour probabilities for drawing contour levels
      cl <- contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
      
      de_fit.m <- data.frame(matrix(data=NA,nrow=length(trait.names),ncol=4))
      de_fit.m[,1] <- pca_FMr$var$coord[,1]
      de_fit.m[,2] <- pca_FMr$var$coord[,2]
      
      
      rownames(de_fit.m) <- rownames(pca_FMr$var$coord)
      colnames(de_fit.m) <- c("PC1","PC2","r2","Pr(>1)")
      # arrows
      act.place.x <- vector(mode="numeric",length=nrow(de_fit.m))
      act.place.y <- vector(mode="numeric",length=nrow(de_fit.m))
      de_fit.m <- cbind(de_fit.m,act.place.x,act.place.y)
      
      
      ylab_now=expression("PC"[2])
      xlab_now="Absolute latitude"
      plot(est, cont=seq(1,100,by=2), display="filled.contour2", add=FALSE, cex.axis=2,cex.lab=2,
           ylab=ylab_now,
           xlab=xlab_now,
           ylim=c(-8,6)
      )
      
      
      plot(est,abs.cont=cl[1], labels=c(0.5),labcex=1, add=TRUE, lwd=2,lty=2, col= "dimgray")
      plot(est,abs.cont=cl[2], labels=c(0.8),labcex=1, add=TRUE, lwd=1.5,lty=3, col= "dimgray")
      plot(est,abs.cont=cl[3], labels=c(0.95),labcex=1, add=TRUE, lwd=.5,lty = 3, col= "dimgray")
      points(PCA_lat$Group.1,PCA_lat$coord.Dim.2,pch=16,cex=2,col="white")
      points(PCA_lat$Group.1,PCA_lat$coord.Dim.2,pch=16,cex=1.5)
      #abline(lm2,col="black",lwd=3)
      abline(lm2_agg,col="black",lwd=3)
      
      
      smr_lm2 <- summary(lm2)
      r2=expression('r'^2* "=")
      text(x = 70,y = 5,labels = r2,cex=2,col = "gray")
      text(x = 77,y = 5,labels = round(smr_lm2$adj.r.squared,digits = 2),cex=2,col = "gray")
      smr_lm2_agg <- summary(lm2_agg)
      text(x = 70,y = 4,labels = r2,cex=2)
      text(x = 77,y = 4,labels = round(smr_lm2_agg$adj.r.squared,digits = 2),cex=2)
      
      
    dev.off()
  
}