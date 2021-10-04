
  require("dplyr")
  

  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  require(FactoMineR)

  
  
  
  #------------------------------------------------------------------------
  # 0.1  Load data
  #------------------------------------------------------------------------
 
  TRY_TTT <- read.csv(file.path(origin,"data","master_matrix","TTT","TRY_pred_TTT_geo17_.csv"))
  TRY_TTT <- TRY_TTT[,-1]
  colnames(TRY_TTT)
  
  trait_agg1_2 <- as.data.frame(TRY_TTT[,7:ncol(TRY_TTT)])
  lat_agg1_2 <- as.numeric(TRY_TTT[,colnames(TRY_TTT)=="Lat"])
  trait_agg1_2_ttt <- as.data.frame(TRY_TTT[grep(TRY_TTT[,2],pattern = "ttt"),7:ncol(TRY_TTT)])
  lat_agg1_2_ttt <- as.numeric(TRY_TTT[grep(TRY_TTT[,2],pattern = "ttt"),colnames(TRY_TTT)=="Lat"])
  
  # binning needed
  lat_bins <- round(abs(as.numeric(lat_agg1_2)),0)
  data_trait <-cbind(lat_bins,trait_agg1_2) 
  data_trait <- as.data.frame(data_trait)

  data_trait_ttt <-cbind(lat_bins,trait_agg1_2)[grep(TRY_TTT[,2],pattern = "ttt"),] 
  data_trait_ttt <- as.data.frame(data_trait_ttt)
  
  new.median.fun  <- function(x) {
    if (is.numeric(x)) {
      return(median(x, na.rm=T))
    } else {
      return(unique(x))
    }
  }
  
  data_trait_ag <- aggregate(x=data_trait[which(!(names(data_trait) %in% c("lat_bins")))], 
                             by = list(data_trait$lat_bins), FUN=new.median.fun)
  data_trait_s <- data_trait_ag[order(data_trait_ag$Group.1),]

  data_trait_agttt <- aggregate(x=data_trait_ttt[which(!(names(data_trait_ttt) %in% c("lat_bins")))], 
                             by = list(data_trait_ttt$lat_bins), FUN=new.median.fun)
  data_trait_sttt <- data_trait_agttt[order(data_trait_agttt$Group.1),]
  
  
  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_10"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_10"))}
  
  
  colz=rep("gray",nrow(TRY_TTT))
    
  #----------------------------------------------------------------------------
  # plot the PC1 against absolute latitude
  #----------------------------------------------------------------------------

  { 
      t=1
      
      for(t in 1:ncol(trait_agg1_2)){
        
        
        if(sum(!is.na(trait_agg1_2_ttt[,t]))!=0){
          png(file=file.path(origin,"figures","Supplement_Fig_10",
                             paste0("figure_S_lat",colnames(trait_agg1_2)[t],"TTT_review.png")))
          par(mfrow=c(1,1),mar=c(5,5,2,2))

          plot(abs(lat_agg1_2),log(as.numeric(trait_agg1_2[,t])),
           xlim=c(0,90),col="gray",xaxt="n",
           ylab=paste0(names(trait_agg1_2)[t]," [log]"),
           xlab="Latitude (absolute)",pch=16,cex=.7,cex.lab=2.5,cex.axis=2)
      
          points(abs(lat_agg1_2_ttt),log(as.numeric(trait_agg1_2_ttt[,t])),
               col="red",pch=16,cex=.7)
      
          axis(1,at = seq(from=0,to = 90,by = 10),labels = c(0,10,20,30,40,50,60,70,80,90),tick = F,cex=2)
          points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=2.5,
                 col="white",pch=16)
          points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=1.5,
                 col="black",pch=16)
      
          points(data_trait_sttt$Group.1,log(data_trait_sttt[,t+1]),cex=2.5,
                 col="white",pch=16)
          points(data_trait_sttt$Group.1,log(data_trait_sttt[,t+1]),cex=1.5,
                 col="orange",pch=16)
          dev.off()
        }
      
      }
      
  }
  
  
    { 
    pdf(file=file.path(origin,"figures","Supplement_Fig_10","figure_S_latTTT_review.pdf"),width =5,height = 3)
    par(mfrow=c(1,1),mar=c(5,5,2,2))
      t=1
    for(t in 1:ncol(trait_agg1_2)){
    if(sum(!is.na(trait_agg1_2_ttt[,t]))!=0){
        
    
    plot(abs(lat_agg1_2),log(as.numeric(trait_agg1_2[,t])),
         xlim=c(0,90),col="gray",xaxt="n",
         ylab=paste0(names(trait_agg1_2)[t]," [log]"),
         xlab="Latitude (absolute)",pch=16,cex=.7,cex.lab=2.5,cex.axis=2)
    
    if(sum(!is.na(trait_agg1_2_ttt[,t]))>1){
      points(abs(lat_agg1_2_ttt),log(as.numeric(trait_agg1_2_ttt[,t])),
           col="red",pch=16,cex=.7)
    }
    
    axis(1,at = seq(from=0,to = 90,by = 10),labels = c(0,10,20,30,40,50,60,70,80,90),tick = F,cex=2)
    points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=2.5,
           col="white",pch=16)
    points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=1.5,
           col="black",pch=16)
    
    if(sum(!is.na(trait_agg1_2_ttt[,t]))){
      points(data_trait_sttt$Group.1,log(data_trait_sttt[,t+1]),cex=2.5,
             col="white",pch=16)
      points(data_trait_sttt$Group.1,log(data_trait_sttt[,t+1]),cex=1.5,
             col="orange",pch=16)
    }
    }
    }
    dev.off()
    
  }
  
  
  { 
    t=1
    for(t in 1:ncol(trait_agg1_2)){
      png(file=file.path(origin,"figures","Supplement_Fig_10",
                         paste0("figure_S_lat",colnames(trait_agg1_2)[t],"TTT_review.png")))
      par(mfrow=c(1,1),mar=c(5,5,2,2))
      
      plot(abs(lat_agg1_2),log(as.numeric(trait_agg1_2[,t])),
           xlim=c(0,90),col="gray",xaxt="n",
           ylab=paste0(names(trait_agg1_2)[t]," [log]"),
           xlab="Latitude (absolute)",pch=16,cex=.7,cex.lab=2.5,cex.axis=2)
      
      if(sum(!is.na(trait_agg1_2_ttt[,t]))>1){
        points(abs(lat_agg1_2_ttt),log(as.numeric(trait_agg1_2_ttt[,t])),
               col="red",pch=16,cex=.7)
      }
      
      axis(1,at = seq(from=0,to = 90,by = 10),labels = c(0,10,20,30,40,50,60,70,80,90),tick = F,cex=2)
      points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=2.5,
             col="white",pch=16)
      points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=1.5,
             col="black",pch=16)
      
      if(sum(!is.na(trait_agg1_2_ttt[,t]))){
        points(data_trait_sttt$Group.1,log(data_trait_sttt[,t+1]),cex=2.5,
               col="white",pch=16)
        points(data_trait_sttt$Group.1,log(data_trait_sttt[,t+1]),cex=1.5,
               col="orange",pch=16)
      }
    
  
    dev.off()
    
  }

  }

  