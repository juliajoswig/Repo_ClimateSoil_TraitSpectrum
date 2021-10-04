output_supplementary <- funtion(){
  # SETUP
  # Julia Joswig
  # 2019_05_01
  # ------------
  
  ### TERMINAL:
  setwd("/Net/Groups")
  ### LOCAL
  orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  #orig_loctem =  "BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  output_term="OLM_data"#
  NA_now="NA_mnNbr"
  # ---------------------------------------------------------------------------------------
  # define the origin path
  # ---------------------------------------------------------------------------------------
  
  # origin = # please add your local path here & comment the ones below.
  origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
  
  # ---------------------------------------------------------------------------------------
  # load data
  # ---------------------------------------------------------------------------------------
  #file.path
  origin_data =file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix")
  if( output_term=="OLM_data"){# ER Agg2 OR
    load(file = file.path(origin_data,"_aggregated_agg2",NA_now,"TRY_Env2_Kier_20190918.RData"))}
  #  load(file = file.path(origin_data,"_aggregated_agg2","TRY_DCS2_WorldClim_Kier.RData"))
  if( output_term=="OLM_Agg1"){  # Grid Agg1 OR
    load(file = file.path(origin_data,"_aggregated_agg1",NA_now,"TRY_Env1grid_20190920.RData"))}
  if( output_term=="OLM_data_Agg2grid"){  # Grid Agg2 OR
    load(file = file.path(origin_data,"_aggregated_agg2",NA_now,"TRY_Env2grid_Kier_20190920.RData"))}
  if( output_term=="OLM_data_Agg2lat"){  # Lat Agg2
    load(file.path(origin_data,"_aggregated_agg2",NA_now,"TRY_Env2lat_20190920.RData"))}
  load(file.path(origin_data,"_aggregated_agg0","TRY","TRY_DCSO_20171123.RData"))
  
  # ---------------------------------------------------------------------------------------
  # define Approach type 
  # ---------------------------------------------------------------------------------------
  # options
  Appr_types = c("Data_GapFilled","Data_Raw","Data_Observed")
  at = 1
  Appr_type_now=Appr_types[at]
  if( output_term=="OLM_data"){TraitEnvDATA <- TRY_Env2_Kier}
  if( output_term=="OLM_data_Agg2lat"| output_term=="OLM_data_Agg2grid"){  TraitEnvDATA <- TRY_Env2}
  if( output_term=="OLM_Agg1"){  TraitEnvDATA <- TRY_Env1}
  
  
  # split into traits and environment (soil & climate)
  source(file.path(origin,"scripts",Appr_types[at],"support_scripts","fn_put_into_DataStream.R"))
  traits_1 <- TraitEnvDATA[put_into_DataStream(names(TraitEnvDATA))=="trait"]
  soil_1 <- TraitEnvDATA[put_into_DataStream(names(TraitEnvDATA))=="soil"]
  climate_1 <- TraitEnvDATA[put_into_DataStream(names(TraitEnvDATA))=="climate"]
  info_1 <- TraitEnvDATA[names(TraitEnvDATA)=="Kier_richness"|
                           names(TraitEnvDATA)=="TER_TRY.ECO_ID"|
                           names(TraitEnvDATA)=="TER_TRY.ECO_NAME"|
                           names(TraitEnvDATA)=="TER_TRY.BIOME"|
                           names(TraitEnvDATA)=="species.count"|
                           names(TraitEnvDATA)=="lat"|
                           names(TraitEnvDATA)=="lon"]
  
  data_master_1 <- cbind(info_1,traits_1,soil_1,climate_1)
  
  # ---------------------------------------------------------------------------------------
  # index data
  # ---------------------------------------------------------------------------------------
  
  # cut out missing ER data
  print(paste("exclude",data_master_1$TER_TRY.ECO_NAME[which(rowSums(is.na(data_master_1))!=0)]))
  data_master_2 <-   data_master_1[-which(rowSums(is.na(data_master_1))!=0),]
  print(paste("no missings",sum(is.na(data_master_2))==0))
  print(paste("total nb ERs",nrow(data_master_2)))
  #data_master_2 = data_master_1
  
  # chose index
  # Kier > 1% & > 20 species per ecoregion
  # no missing data among traits or environment
  # for soil EXcluding worldgrids  # for soil INCLUDING worldgrids: not yet implemented
  
  ix <- data_master_2$species.count > data_master_2$Kier_richness * 0.01 & data_master_2$species.count > 20
  #ix <- data_master_2$species.count  > 20
  #ix <- rep(TRUE,nrow(data_master_2))
  print(paste0("We keep ",sum(ix)," Ecoregions, with ", ncol(soil_1), " soil- and ", ncol(climate_1)," climate factors."))
  #print(paste0("We keep ",sum(ix)," Species, with ", ncol(soil_1), " soil- and ", ncol(climate_1)," climate factors."))
  #print(paste0("We keep ",sum(ix)," grids, with ", ncol(soil_1), " soil- and ", ncol(climate_1)," climate factors."))
  
  # ---------------------------------------------------------------------------------------
  #  Prediction
  # ---------------------------------------------------------------------------------------
  soil_2 <- data_master_2[ix,put_into_DataStream(names(data_master_2))=="soil"]
  climate_2 <- data_master_2[ix,put_into_DataStream(names(data_master_2))=="climate"]
  trait_2 <- data_master_2[ix,put_into_DataStream(names(data_master_2))=="trait"]
  info_2 <- data_master_2[ix,names(data_master_2)=="Kier_richness"|
                            names(data_master_2)=="TER_TRY.ECO_ID"|
                            names(data_master_2)=="TER_TRY.ECO_NAME"|
                            names(data_master_2)=="TER_TRY.BIOME"|
                            names(data_master_2)=="species.count"|
                            names(data_master_2)=="lat"|
                            names(data_master_2)=="lon"]
  noise=matrix(NA,ncol=ncol(soil_2),nrow=(nrow(soil_2)))
  colnames(noise) = paste0("noise",1:ncol(noise))
  for(i in 1:ncol(soil_2)){
    noise[,i]= rnorm(nrow(soil_2),mean = sample(x = c(-5:+5),size = 1),sd= sample(c(.5,.4,.3,.2,.1,5,4,3,2,1),size = 1)) 
  }
  
  names(soil_2)
  names(climate_2)
  info_2$species.count
  print(paste0("per ecoregion mean of ", round(mean(info_2$species.count),digits = 0)," species."))
  print(paste0("per ecoregion MAX of ", max(info_2$species.count)," species."))
  print(paste0("MAX nb os species has ", info_2$TER_TRY.ECO_NAME[which(info_2$species.count==max(info_2$species.count))]))
  print(paste0("all ecoregion SUM of ", sum(info_2$species.count)," species."))
  
  print("-----------------------------")
 print(paste0("In total, we included ",sum(TRY_DCSO$TER_TRY.ECO_ID%in%info_2$TER_TRY.ECO_ID)," observations.") )
 print(paste0("In total, we included ",length(unique(TRY_DCSO$Species[which(TRY_DCSO$TER_TRY.ECO_ID%in%info_2$TER_TRY.ECO_ID)]))," species.") )
 print(paste0("In total, we included ",length(unique(TRY_DCSO$Genus[which(TRY_DCSO$TER_TRY.ECO_ID%in%info_2$TER_TRY.ECO_ID)]))," genera.") )
 print(paste0("In total, we included ",length(unique(TRY_DCSO$Family[which(TRY_DCSO$TER_TRY.ECO_ID%in%info_2$TER_TRY.ECO_ID)]))," families.") )
 
 
 
}