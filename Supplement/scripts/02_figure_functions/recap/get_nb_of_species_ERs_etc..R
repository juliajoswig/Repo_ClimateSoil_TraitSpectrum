# SETUP
# Julia Joswig
# 2019_05_01
# ------------

### TERMINAL:
  setwd("/Net/Groups")
### LOCAL
  orig_loctem =  "BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
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
  # ER Agg2 OR
  load(file = file.path(origin_data,"_aggregated_agg2",NA_now,"TRY_Env2_Kier_20190918.RData"))
  # Grid Agg1ER OR
  load(file = file.path(origin_data,"_aggregated_agg1",NA_now,"TRY_Env1_20190918.RData"))
  load(file = file.path(origin_data,"_aggregated_agg0","TRY","TRY_Env_20190918.RData"))
  
# ---------------------------------------------------------------------------------------
# define Approach type 
# ---------------------------------------------------------------------------------------
  # options
  Appr_types = c("Data_GapFilled","Data_Raw","Data_Observed")
  at = 1
  Appr_type_now=Appr_types[at]

  source(file.path(origin,"scripts",Appr_types[at],"support_scripts","fn_put_into_DataStream.R"))

  # split into traits and environment (soil & climate)
  TraitEnvDATA <- TRY_Env2_Kier
#  TraitEnvDATA <- TRY_Env2
#  TraitEnvDATA <- TRY_Env1
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
#  data_master_2 = data_master_1
  
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
  Ecoregions_to_keep <- data_master_2$TER_TRY.ECO_NAME[ix]
  
  length(unique(TRY_Env1$Group.1[TRY_Env1$Group.2%in%Ecoregions_to_keep]))
  length(TRY_Env$Species[TRY_Env$TER_TRY.ECO_NAME%in%Ecoregions_to_keep])
  
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

  
  #----------------------------------------------------------------------------------------
  # CARS-pls
  #----------------------------------------------------------------------------------------
  nruns=50
#  climOsoil="soil" # soil _ only
#  climOsoil="climate" # climate _ only
  climOsoil="soilAclimate" # soilAclimate _ only
  # test data
#  climOsoil="noise" # noise _ only
#  climOsoil="noiseAclimate"
#  climOsoil="soilAnoise"
#  climOsoil="soilAclimateAnoise"

  # check if analysis exists:
  if(file.exists(file.path(origin, "data", "Results","CARS_PLS","VarSelect",climOsoil, paste0(nruns,"Reps"),
                        paste0("VarSelOutput",output_term,"_",Appr_type_now,".RData")))){
    print(paste("CARS-pls",climOsoil,"file exists"))}else{print("file does not exist.")}
    print(climOsoil)
   source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","CARS_PLS","analysis_CARS_pls.R"))
    result_CARS_pls <- analysis_CARS_pls(trait_2,soil_2,climate_2,noise,origin,Appr_types[at],climOsoil=climOsoil,nruns=nruns,output_term)

 