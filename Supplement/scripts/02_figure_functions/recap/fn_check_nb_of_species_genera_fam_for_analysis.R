check_nb_of_species_genera_fam_for_analysis <- function(){
  
  # load obs level data
  load(file.path(origin,"data","Aggregations","SpatialAggr",paste0(versionNOW,"_Agg0"),"TRY_DCSO.RData"))
  
  TRY_
  #load data which eventually got selected
  xi=4
  yi=1
  atmObio = Xs[xi]
  meanOsd = Ys[yi]
  
  print(paste0(atmObio,"_",meanOsd,"_",nruns,"reps_latentVectors:",ncomp,"Aggregation here:",Agg_type,sel_now))
  
  if(Agg_type=="V2_Agg2lin_"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp_lin.R"))}
  if(Agg_type=="V2_SoilFromClim"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp_SfC.R"))}
  if(Agg_type=="V2_Agg2Tcor_"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp_Tcor.R"))}
  if(Agg_type=="V2_Agg2lin250_"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp_lin250.R"))}
  if(Agg_type=="V2_Agg2NONlin_"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp.R"))}
  if(Agg_type=="V2_Agg2linScale_"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp_linScaled.R"))}
  if(Agg_type=="V2_Agg2WoodyNon_"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp_WoodyNon.R"))}
  if(Agg_type=="V2_Agg2lin_NULL_"){source(file.path(origin,"scripts","analysis","V2","data_process","fn_createpredAresp_lin_NULL.R"))}
  out <- createpredAresp(origin,meanOsd,atmObio,trait.names,Agg_type,sel_now,versionNOW)
  sum(put_into_soilOrAtm(names(out$pred))=="soil")
  names(out$pred)[put_into_soilOrAtm(names(out$pred))=="soil"]
  names(out$pred)[put_into_soilOrAtm(names(out$pred))=="atm"]
  names(out$pred)[put_into_soilOrAtm(names(out$pred))!="atm"&put_into_soilOrAtm(names(out$pred))!="soil"]
  put_into_soilOrAtm(names(out$pred))
  dim(out$pred)
  names(out$pred)
  
  # get only those ERs which enter the analysis
  TRY_cut <- TRY_DCSO[TRY_DCSO$TER_TRY.ECO_ID%in%out$er.NB3$Eco_ID,]  
  print(paste(nrow(TRY_cut),"geo-referenced observations"))
  print(paste(length(unique(TRY_cut$AccSpeciesID)),"unique species"))
  print(paste(length(unique(TRY_cut$Genus)),"unique genera"))
  print(paste(length(unique(TRY_cut$Family)),"unique Families"))
  
  # load spec level data
  load(file.path(origin,"data","Aggregations","SpatialAggr",paste0(versionNOW,"_Agg1_","SpeciesMedianPerER"),paste0("TRY_DCS1.RData")))
  # get only those ERs which enter the analysis
  TRY1_cut <- TRY_DCS1[TRY_DCS1$TER_TRY.ECO_ID%in%out$er.NB3$Eco_ID,]  
  print(paste(nrow(TRY1_cut),"geo-referenced observations"))
  new.count.fun <- function(x){
    return(length(x))
  }
  
  TRY1_sp = aggregate(x=TRY1_cut[,which((names(TRY1_cut) 
                                          %in% c("AccSpeciesID")))],
                       by = list(TRY1_cut$Group.1), 
                       FUN=new.count.fun)
  
  print(paste(mean(TRY1_sp$x),"species per ER"))
  print(paste(max(TRY1_sp$x),"maximum number of species per ER"))

}
27+121
