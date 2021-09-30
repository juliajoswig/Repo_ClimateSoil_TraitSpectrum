write_PCA_info_into_csv <- fuction(){
  nruns=100
  atmObio="soilAatmosphere"
  
  
  load(file.path(origin,"data","Helper_files","PCA",paste0("totPRED_Agg2_",meanOsd,".RData")))
  
  pca_pred2 <- pca_pred2_l$pca_pred2
  pca_pred2_coord <- pca_pred2$ind$coord
  pca_pred2_loading <- pca_pred2$var$coord
  write.csv(pca_pred2_loading,file=file.path(origin,"data","pca_pred2.csv"))  
  
  # add the relative importances for each trait
  # add the r2 for each trait
  # load
  load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),"summary",paste0(atmObio,"_trait_",meanOsd,"_",Agg_type,sel_now,"_RiverInput_r2weighted.RData")))
  head(df_final2)
  t=1
  types=unique(df_final2$trait)
  #rename
  relationship<- as.vector(df_final2$relationship)
  relationship <- gsub(relationship,pattern = "M_sl",replacement = "")
  relationship <- gsub(relationship,pattern = "M_sd",replacement = "")
  relationship <- gsub(relationship,pattern = "_ll.tif_mean",replacement = "")
  relationship <- gsub(relationship,pattern = "mean_mean",replacement = "mean")
  df_final2$relationship <- relationship
  
  t=1
  for(t in 1:length(types)){
    trait_tmp <- df_final2[df_final2$trait%in%types[t],]  
    new_col <- rep(NA,nrow(pca_pred2_coord))
    new_colw <- rep(NA,nrow(pca_pred2_coord))
    i=1
      for(i in 1:nrow(pca_pred2_coord)){
        ix <- paste0(trait_tmp$driver,"_",trait_tmp$relationship)%in%rownames(pca_pred2_coord)[i]
        if(sum(ix)!=0){
        print(paste0(trait_tmp$driver,"_",trait_tmp$relationship)[ix])
        print(sum(as.numeric(as.vector(trait_tmp$rel_Importance[ix]))))
        new_col[i] <- sum(as.numeric(as.vector(trait_tmp$rel_Importance[ix])))
        new_colw[i] <- sum(as.numeric(as.vector(trait_tmp$rel_Importance[ix]))*unique(as.numeric(as.vector(trait_tmp$r2[ix]))))
        
        rm(ix)
        }
      }
    
    pca_pred2_coord<-  cbind(pca_pred2_coord,new_col)
    colnames(pca_pred2_coord)[ncol(pca_pred2_coord)] <- types[t]
    pca_pred2_coord<-  cbind(pca_pred2_coord,new_colw)
    colnames(pca_pred2_coord)[ncol(pca_pred2_coord)] <- paste0(types[t],"_weighted_withr2")
  }
  
  #add soil or climate attribution
  new_col <- rep(NA,nrow(pca_pred2_coord))
  pca_pred2_coord <- cbind(put_into_soilOrAtm(rownames(pca_pred2_coord)),pca_pred2_coord)
  colnames(pca_pred2_coord)[1] <- "Variable_Type"
  pca_pred2_coord<- as.data.frame(pca_pred2_coord)
  head(pca_pred2_coord)
  
  # test plot
  plot(pca_pred2_coord$) 
  
  write.csv(pca_pred2_coord,file=file.path(origin,"data","PCA_drivers.csv"))
            
            
  pca_pred2_coord_t <-   t(pca_pred2_coord[,2:ncol(pca_pred2_coord)])
  pairs(pca_pred2_coord_t[,100:114])
  PCA(pca_pred2_coord_t)
  
  dev.off()
  plot(pca_pred2_coord$Dim.1[pca_pred2_coord$Variable_Type=="soil"],pca_pred2_coord$SLA[pca_pred2_coord$Variable_Type=="soil"])
  plot(pca_pred2_coord$Dim.1[pca_pred2_coord$Variable_Type=="atm"],pca_pred2_coord$SLA[pca_pred2_coord$Variable_Type=="atm"])
  plot(pca_pred2_coord$Dim.3,pca_pred2_coord$SLA)
  head( pca_pred2_coord)
}