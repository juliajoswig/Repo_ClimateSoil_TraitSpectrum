Tab_S2 <- function(origin,trait.names,Agg_type,sel_now){
  
  #-----------------------------
  # load ER info
  #-----------------------------
  load(file.path(origin,"data","Helper_files","KierRichnness.RData"))
  dim(KierRichness)
  
  #-----------------------------
  # load selected ecoregion
  #-----------------------------
  # get the Ecoregions of interest:
  source(file.path(origin,"scripts","data_process","fn_createpredAresp_lin.R"))
  out <- createpredAresp(origin,meanOsd="median",atmObio="soilAatmosphere",trait.names,Agg_type,sel_now)
  selected_ER <- cbind(out$er.NB3$Eco_ID,out$er.NB3$LeArea_info)
  colnames(selected_ER) <- c("ER","nb of traits")
  head(selected_ER)

  ER_infos <- KierRichness[KierRichness$ER%in%selected_ER[,1],]
  ER_infos2 <- cbind(ER_infos,selected_ER[match(x = ER_infos$ER,table = selected_ER[,1]),2])
  colnames(ER_infos2)[ncol(ER_infos2)] <- "number of trait observations"
  head(ER_infos2)
  ER_tab <- ER_infos2[order(ER_infos2$Lat_Bbox,decreasing = T),c(3,17,10,1,8,9,13)]
  ER_tab$Long_Bbox <- round(ER_tab$Long_Bbox,digits = 1)
  ER_tab$Lat_Bbox <- round(ER_tab$Lat_Bbox,digits = 1)
  ER_tab <- as.data.frame(ER_tab)
  
  #---------------------
  # write csv
  #---------------------
  write.csv(x = ER_tab,file=file.path(origin,"tabs","Tab_S2","Tab_S2_ER.csv"))
  
  #  plot(ER_tab$Lat_Bbox,ER_tab$Plant.Richness)
#  plot(ER_tab$Lat_Bbox,ER_tab$`number of trait observations`)
}