fn_statistics_on_input_data <- function(){
  
  
  load(file=file.path("infos"))
  load(file=file.path("pred"))
  load(file=file.path("resp"))
  load(file.path(origin,"data","SpatialAggr","V6_Agg0","TRY_DCSO.RData"))
  
  TRY_DCSO_ActERs <- TRY_DCSO[TRY_DCSO$TER_TRY.ECO_ID%in%infos$Eco_ID,]
  dim(TRY_DCSO_ActERs)  
  
  print(paste0("nb of ecoregions : ",nrow(resp)))
  print(paste0("average nb of species per ecoregions : ",mean(infos$SLA_info)))
  print(paste0("median nb of species per ecoregions : ",median(infos$SLA_info)))
  print(paste0("min nb of species per ecoregions : ",min(infos$SLA_info)))
  print(paste0("max nb of species per ecoregions : ",max(infos$SLA_info)))
  
  print(paste0("nb of individual plants : ",nrow(TRY_DCSO_ActERs)))
  print(paste0("nb of species : ",length(unique(TRY_DCSO_ActERs$Species))))
  print(paste0("nb of genera : ",length(unique(TRY_DCSO_ActERs$Genus))))
 summary(factor(TRY_DCSO_ActERs$Genus))
  print(paste0("nb of families : ",length(unique(TRY_DCSO_ActERs$Family))))
 
  dim(pred) 
}