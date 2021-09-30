Rename_Vars <- function(input){

  #define names to be filtered from table-names
  # trait_nms <- c("LeArea","SSD","SLA","LeC","LeN_","LeP","PlantHeight","SeedMass","SeLen","LeNArea",
  #                 "LeNP","Led15N","SenbU","LeFMass","ConduitDens","DispULen","VesLen")

  transl_s <-matrix(NA,ncol=2,nrow=length(input)) 
  transl_s[,1] <- input#c(SG_n,OLM_n,climate_fcts)
  transl_s_sd <- transl_s
  transl_c <- transl_s # climate variables
  transl_c_sd <- transl_s # climate variables
  transl_t <- transl_s # trait names
  transl_t_sd<- transl_s # trait names but sd
  transl_a <- transl_s # 
  transl_a1 <- transl_s # 
  transl_a2 <- transl_s # 
  transl_a3 <- transl_s # 
  transl_d <- transl_s #
  transl_b <- transl_s #
  
  transl_b[which(transl_b[,1]%in%"1"),2] <- "Tropical Subtropical Moist Broadleaf Forests"
  transl_b[which(transl_b[,1]%in%"2"),2] <- "Tropical Subtropical Dry Broadleaf Forests"
  transl_b[which(transl_b[,1]%in%"3"),2] <- "Tropical Subtropical Coniferous Broadleaf Forests"
  transl_b[which(transl_b[,1]%in%"4"),2] <- "Temperate Broadleaf Mixed Forests"
  transl_b[which(transl_b[,1]%in%"5"),2] <- "Temperate Coniferous Forests"
  transl_b[which(transl_b[,1]%in%"6"),2] <- "Boreal Forests Taiga"
  transl_b[which(transl_b[,1]%in%"7"),2] <- "Tropical Subtropical Grasslands, Savannas, Shrubland"
  transl_b[which(transl_b[,1]%in%"8"),2] <- "Temperate Grasslands, Savannas, Shrubland"
  transl_b[which(transl_b[,1]%in%"9"),2] <- "Flooded Grasslands and Savannas"
  transl_b[which(transl_b[,1]%in%"10"),2] <- "Montane Grasslands Shrublands"
  transl_b[which(transl_b[,1]%in%"11"),2] <- "Tundra"
  transl_b[which(transl_b[,1]%in%"12"),2] <- "Mediterranean Forests, Woodlands, Scrub"
  transl_b[which(transl_b[,1]%in%"13"),2] <- "Desert and Xeric Shrublands"
  transl_b[which(transl_b[,1]%in%"14"),2] <- "Mangroves"
  
  
  # traits 1/2  as in TRY data base
  transl_t[which(transl_t[,1]%in%"X1"),2] <- "LeArea" 
  transl_t[which(transl_t[,1]%in%"X4"),2] <- "SSD" 
  transl_t[which(transl_t[,1]%in%"X11"),2] <- "SLA" 
  transl_t[which(transl_t[,1]%in%"X13"),2] <- "LeC"
  transl_t[which(transl_t[,1]%in%"X14"),2] <- "LeN" 
  transl_t[which(transl_t[,1]%in%"X15"),2] <- "LeP" 
  transl_t[which(transl_t[,1]%in%"X18"),2] <- "PlantHeight" 
  transl_t[which(transl_t[,1]%in%"X26"),2] <- "SeedMass" 
  transl_t[which(transl_t[,1]%in%"X27"),2] <- "SeLen" 
  transl_t[which(transl_t[,1]%in%"X50"),2] <- "LeNArea" 
  transl_t[which(transl_t[,1]%in%"X56"),2] <- "LeNP" 
  transl_t[which(transl_t[,1]%in%"X78"),2] <- "Led15N" 
  transl_t[which(transl_t[,1]%in%"X138"),2] <- "SenbU" 
  transl_t[which(transl_t[,1]%in%"X163"),2] <- "LeFMass" 
  transl_t[which(transl_t[,1]%in%"X169"),2] <- "ConduitDens" 
  transl_t[which(transl_t[,1]%in%"X237"),2] <- "DispULen" 
  transl_t[which(transl_t[,1]%in%"X282"),2] <- "VesLen" 


  
  #traits 2/2
  transl_t[which(transl_t[,1]%in%"LeArea"),2] <- "Leaf Area" 
  transl_t[which(transl_t[,1]%in%"Led15N"),2] <- "Delta 15 N"
  transl_t[which(transl_t[,1]%in%"LeNArea"),2] <- "Leaf N per Area"
  transl_t[which(transl_t[,1]%in%"LeNP"),2] <- "Leaf N:P ratio"
  transl_t[which(transl_t[,1]%in%"LeC"),2] <- "Leaf C"
  transl_t[which(transl_t[,1]%in%"LeP"),2] <- "Leaf P" 
  transl_t[which(transl_t[,1]%in%"LeN"),2] <- "Leaf N" 
  transl_t[which(transl_t[,1]%in%"SLA"),2] <- "SLA" 
  transl_t[which(transl_t[,1]%in%"ConduitDens"),2] <- "Conduit density" 
  transl_t[which(transl_t[,1]%in%"LeFMass"),2] <- "Leaf fresh mass" 
  transl_t[which(transl_t[,1]%in%"SenbU"),2] <- "Seeds per Reprod U" 
  transl_t[which(transl_t[,1]%in%"PlantHeight"),2] <- "Height" 
  transl_t[which(transl_t[,1]%in%"SeLen"),2] <- "Seed length" 
  transl_t[which(transl_t[,1]%in%"VesLen"),2] <- "Vessel el. length" 
  transl_t[which(transl_t[,1]%in%"DispULen"),2] <- "Dispersal U length" 
  transl_t[which(transl_t[,1]%in%"SeedMass"),2] <- "Seed mass"  
  transl_t[which(transl_t[,1]%in%"SSD"),2] <- "Stem Density" 

    
  #------------------
  transl_c_sd[grep(transl_c_sd[,1],pattern = "PCA_sd_CLIMATE"),2] <- "climate_sd"
  transl_s_sd[grep(transl_c_sd[,1],pattern = "PCA_sd_SOIL"),2] <-  "soil_sd"
  
  # soil  

  transl_s[which(transl_s[,1]%in%"ORCDRC_M_sl1_1km_ll.tif"),2] <- "C_concentration" 
  transl_s[which(transl_s[,1]%in%"ORCDRC_M_sl2_1km_ll.tif"),2] <- "C_concentration_2" 
  transl_s[which(transl_s[,1]%in%"ORCDRC_M_sl3_1km_ll.tif"),2] <- "C_concentration_3" 
  transl_s[which(transl_s[,1]%in%"ORCDRC_M_sl4_1km_ll.tif"),2] <- "C_concentration_4" 
  transl_s[which(transl_s[,1]%in%"ORCDRC_M_sl5_1km_ll.tif"),2] <- "C_concentration_5" 
  transl_s[which(transl_s[,1]%in%"ORCDRC_M_sl6_1km_ll.tif"),2] <- "C_concentration_6" 
  transl_s[which(transl_s[,1]%in%"ORCDRC_M_sl7_1km_ll.tif"),2] <- "C_concentration_7" 
  
  transl_s[which(transl_s[,1]%in%"BLDFIE_M_sl1_1km_ll.tif"),2] <- "Density" 
  transl_s[which(transl_s[,1]%in%"BLDFIE_M_sl2_1km_ll.tif"),2] <- "Density_2" 
  transl_s[which(transl_s[,1]%in%"BLDFIE_M_sl3_1km_ll.tif"),2] <- "Density_3" 
  transl_s[which(transl_s[,1]%in%"BLDFIE_M_sl4_1km_ll.tif"),2] <- "Density_4" 
  transl_s[which(transl_s[,1]%in%"BLDFIE_M_sl5_1km_ll.tif"),2] <- "Density_5" 
  transl_s[which(transl_s[,1]%in%"BLDFIE_M_sl6_1km_ll.tif"),2] <- "Density_6" 
  transl_s[which(transl_s[,1]%in%"BLDFIE_M_sl7_1km_ll.tif"),2] <- "Density_7" 
  
  transl_s[which(transl_s[,1]%in%"AWCtS_M_sl1_1km_ll.tif"),2] <- "WaterSat" 
  transl_s[which(transl_s[,1]%in%"AWCtS_M_sl2_1km_ll.tif"),2] <- "WaterSat_2" 
  transl_s[which(transl_s[,1]%in%"AWCtS_M_sl3_1km_ll.tif"),2] <- "WaterSat_3" 
  transl_s[which(transl_s[,1]%in%"AWCtS_M_sl4_1km_ll.tif"),2] <- "WaterSat_4" 
  transl_s[which(transl_s[,1]%in%"AWCtS_M_sl5_1km_ll.tif"),2] <- "WaterSat_5" 
  transl_s[which(transl_s[,1]%in%"AWCtS_M_sl6_1km_ll.tif"),2] <- "WaterSat_6" 
  transl_s[which(transl_s[,1]%in%"AWCtS_M_sl7_1km_ll.tif"),2] <- "WaterSat_7" 
  
  transl_s[which(transl_s[,1]%in%"AWCh1_M_sl1_1km_ll.tif"),2] <- "WaterPot10" 
  transl_s[which(transl_s[,1]%in%"AWCh1_M_sl2_1km_ll.tif"),2] <- "WaterPot10_2" 
  transl_s[which(transl_s[,1]%in%"AWCh1_M_sl3_1km_ll.tif"),2] <- "WaterPot10_3" 
  transl_s[which(transl_s[,1]%in%"AWCh1_M_sl4_1km_ll.tif"),2] <- "WaterPot10_4" 
  transl_s[which(transl_s[,1]%in%"AWCh1_M_sl5_1km_ll.tif"),2] <- "WaterPot10_5" 
  transl_s[which(transl_s[,1]%in%"AWCh1_M_sl6_1km_ll.tif"),2] <- "WaterPot10_6" 
  transl_s[which(transl_s[,1]%in%"AWCh1_M_sl7_1km_ll.tif"),2] <- "WaterPot10_7" 
  
  transl_s[which(transl_s[,1]%in%"AWCh2_M_sl1_1km_ll.tif"),2] <- "WaterPot20" 
  transl_s[which(transl_s[,1]%in%"AWCh2_M_sl2_1km_ll.tif"),2] <- "WaterPot20_2" 
  transl_s[which(transl_s[,1]%in%"AWCh2_M_sl3_1km_ll.tif"),2] <- "WaterPot20_3" 
  transl_s[which(transl_s[,1]%in%"AWCh2_M_sl4_1km_ll.tif"),2] <- "WaterPot20_4" 
  transl_s[which(transl_s[,1]%in%"AWCh2_M_sl5_1km_ll.tif"),2] <- "WaterPot20_5" 
  transl_s[which(transl_s[,1]%in%"AWCh2_M_sl6_1km_ll.tif"),2] <- "WaterPot20_6" 
  transl_s[which(transl_s[,1]%in%"AWCh2_M_sl7_1km_ll.tif"),2] <- "WaterPot20_7" 
  
  transl_s[which(transl_s[,1]%in%"AWCh3_M_sl1_1km_ll.tif"),2] <- "WaterPot32" 
  transl_s[which(transl_s[,1]%in%"AWCh3_M_sl2_1km_ll.tif"),2] <- "WaterPot32_2" 
  transl_s[which(transl_s[,1]%in%"AWCh3_M_sl3_1km_ll.tif"),2] <- "WaterPot32_3" 
  transl_s[which(transl_s[,1]%in%"AWCh3_M_sl4_1km_ll.tif"),2] <- "WaterPot32_4" 
  transl_s[which(transl_s[,1]%in%"AWCh3_M_sl5_1km_ll.tif"),2] <- "WaterPot32_5" 
  transl_s[which(transl_s[,1]%in%"AWCh3_M_sl6_1km_ll.tif"),2] <- "WaterPot32_6" 
  transl_s[which(transl_s[,1]%in%"AWCh3_M_sl7_1km_ll.tif"),2] <- "WaterPot32_7" 
  
  
  transl_s[which(transl_s[,1]%in%"WWP_M_sl1_1km_ll.tif"),2] <- "WaterWilt" 
  transl_s[which(transl_s[,1]%in%"WWP_M_sl2_1km_ll.tif"),2] <- "WaterWilt_2" 
  transl_s[which(transl_s[,1]%in%"WWP_M_sl3_1km_ll.tif"),2] <- "WaterWilt_3" 
  transl_s[which(transl_s[,1]%in%"WWP_M_sl4_1km_ll.tif"),2] <- "WaterWilt_4" 
  transl_s[which(transl_s[,1]%in%"WWP_M_sl5_1km_ll.tif"),2] <- "WaterWilt_5" 
  transl_s[which(transl_s[,1]%in%"WWP_M_sl6_1km_ll.tif"),2] <- "WaterWilt_6" 
  transl_s[which(transl_s[,1]%in%"WWP_M_sl7_1km_ll.tif"),2] <- "WaterWilt_7" 
  
  transl_s[which(transl_s[,1]%in%"SNDPPT_M_sl1_1km_ll.tif"),2] <- "Sand" 
  transl_s[which(transl_s[,1]%in%"SNDPPT_M_sl2_1km_ll.tif"),2] <- "Sand_2" 
  transl_s[which(transl_s[,1]%in%"SNDPPT_M_sl3_1km_ll.tif"),2] <- "Sand_3" 
  transl_s[which(transl_s[,1]%in%"SNDPPT_M_sl4_1km_ll.tif"),2] <- "Sand_4" 
  transl_s[which(transl_s[,1]%in%"SNDPPT_M_sl5_1km_ll.tif"),2] <- "Sand_5" 
  transl_s[which(transl_s[,1]%in%"SNDPPT_M_sl6_1km_ll.tif"),2] <- "Sand_6" 
  transl_s[which(transl_s[,1]%in%"SNDPPT_M_sl7_1km_ll.tif"),2] <- "Sand_7" 
  
  transl_s[which(transl_s[,1]%in%"SLTPPT_M_sl1_1km_ll.tif"),2] <- "Silt" 
  transl_s[which(transl_s[,1]%in%"SLTPPT_M_sl2_1km_ll.tif"),2] <- "Silt_2" 
  transl_s[which(transl_s[,1]%in%"SLTPPT_M_sl3_1km_ll.tif"),2] <- "Silt_3" 
  transl_s[which(transl_s[,1]%in%"SLTPPT_M_sl4_1km_ll.tif"),2] <- "Silt_4" 
  transl_s[which(transl_s[,1]%in%"SLTPPT_M_sl5_1km_ll.tif"),2] <- "Silt_5" 
  transl_s[which(transl_s[,1]%in%"SLTPPT_M_sl6_1km_ll.tif"),2] <- "Silt_6" 
  transl_s[which(transl_s[,1]%in%"SLTPPT_M_sl7_1km_ll.tif"),2] <- "Silt_7" 
  

  transl_s[which(transl_s[,1]%in%"PHIKCL_M_sl1_1km_ll.tif"),2] <- "pH" 
  transl_s[which(transl_s[,1]%in%"PHIKCL_M_sl2_1km_ll.tif"),2] <- "pH_2"
  transl_s[which(transl_s[,1]%in%"PHIKCL_M_sl3_1km_ll.tif"),2] <- "pH_3" 
  transl_s[which(transl_s[,1]%in%"PHIKCL_M_sl4_1km_ll.tif"),2] <- "pH_4" 
  transl_s[which(transl_s[,1]%in%"PHIKCL_M_sl5_1km_ll.tif"),2] <- "pH_5" 
  transl_s[which(transl_s[,1]%in%"PHIKCL_M_sl6_1km_ll.tif"),2] <- "pH_6" 
  transl_s[which(transl_s[,1]%in%"PHIKCL_M_sl7_1km_ll.tif"),2] <- "pH_7" 

  transl_s[which(transl_s[,1]%in%"OCDENS_M_sl1_1km_ll.tif"),2] <- "C_density" 
  transl_s[which(transl_s[,1]%in%"OCDENS_M_sl2_1km_ll.tif"),2] <- "C_density_2" 
  transl_s[which(transl_s[,1]%in%"OCDENS_M_sl3_1km_ll.tif"),2] <- "C_density_3" 
  transl_s[which(transl_s[,1]%in%"OCDENS_M_sl4_1km_ll.tif"),2] <- "C_density_4" 
  transl_s[which(transl_s[,1]%in%"OCDENS_M_sl5_1km_ll.tif"),2] <- "C_density_5" 
  transl_s[which(transl_s[,1]%in%"OCDENS_M_sl6_1km_ll.tif"),2] <- "C_density_6" 
  transl_s[which(transl_s[,1]%in%"OCDENS_M_sl7_1km_ll.tif"),2] <- "C_density_7" 
  
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_sd1_1km_ll.tif"),2] <- "C_stock" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_sd2_1km_ll.tif"),2] <- "C_stock_2" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_sd3_1km_ll.tif"),2] <- "C_stock_3" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_sd4_1km_ll.tif"),2] <- "C_stock_4" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_sd5_1km_ll.tif"),2] <- "C_stock_5" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_sd6_1km_ll.tif"),2] <- "C_stock_6" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_sd7_1km_ll.tif"),2] <- "C_stock_7" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_30cm_1km_ll.tif"),2] <- "C_stocki_0.3m" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_100cm_1km_ll.tif"),2] <- "C_stocki_1m" 
  transl_s[which(transl_s[,1]%in%"OCSTHA_M_200cm_1km_ll.tif"),2] <- "C_stocki_2m" 
  
  transl_s[which(transl_s[,1]%in%"CRFVOL_M_sl1_1km_ll.tif"),2] <- "Gravel" 
  transl_s[which(transl_s[,1]%in%"CRFVOL_M_sl2_1km_ll.tif"),2] <- "Gravel_2" 
  transl_s[which(transl_s[,1]%in%"CRFVOL_M_sl3_1km_ll.tif"),2] <- "Gravel_3" 
  transl_s[which(transl_s[,1]%in%"CRFVOL_M_sl4_1km_ll.tif"),2] <- "Gravel_4" 
  transl_s[which(transl_s[,1]%in%"CRFVOL_M_sl5_1km_ll.tif"),2] <- "Gravel_5" 
  transl_s[which(transl_s[,1]%in%"CRFVOL_M_sl6_1km_ll.tif"),2] <- "Gravel_6" 
  transl_s[which(transl_s[,1]%in%"CRFVOL_M_sl7_1km_ll.tif"),2] <- "Gravel_7"
  
  transl_s[which(transl_s[,1]%in%"CLYPPT_M_sl1_1km_ll.tif"),2] <- "Clay" 
  transl_s[which(transl_s[,1]%in%"CLYPPT_M_sl2_1km_ll.tif"),2] <- "Clay_2" 
  transl_s[which(transl_s[,1]%in%"CLYPPT_M_sl3_1km_ll.tif"),2] <- "Clay_3" 
  transl_s[which(transl_s[,1]%in%"CLYPPT_M_sl4_1km_ll.tif"),2] <- "Clay_4" 
  transl_s[which(transl_s[,1]%in%"CLYPPT_M_sl5_1km_ll.tif"),2] <- "Clay_5" 
  transl_s[which(transl_s[,1]%in%"CLYPPT_M_sl6_1km_ll.tif"),2] <- "Clay_6" 
  transl_s[which(transl_s[,1]%in%"CLYPPT_M_sl7_1km_ll.tif"),2] <- "Clay_7" 
  
  transl_s[which(transl_s[,1]%in%"CECSOL_M_sl1_1km_ll.tif"),2] <- "CEC" 
  transl_s[which(transl_s[,1]%in%"CECSOL_M_sl2_1km_ll.tif"),2] <- "CEC_2" 
  transl_s[which(transl_s[,1]%in%"CECSOL_M_sl3_1km_ll.tif"),2] <- "CEC_3" 
  transl_s[which(transl_s[,1]%in%"CECSOL_M_sl4_1km_ll.tif"),2] <- "CEC_4" 
  transl_s[which(transl_s[,1]%in%"CECSOL_M_sl5_1km_ll.tif"),2] <- "CEC_5" 
  transl_s[which(transl_s[,1]%in%"CECSOL_M_sl6_1km_ll.tif"),2] <- "CEC_6" 
  transl_s[which(transl_s[,1]%in%"CECSOL_M_sl7_1km_ll.tif"),2] <- "CEC_7" 
  
 
#---------------
#  addition soil
#-------------
  
  
  transl_a[grep(transl_s[,2],pattern = "Density"),2] <- "soil_physics"
  transl_a[grep(transl_s[,2],pattern = "WaterWilt"),2] <- "soil_physics"
  transl_a[grep(transl_s[,2],pattern = "WaterPot"),2] <- "soil_physics"
  transl_a[grep(transl_s[,2],pattern = "WaterSat"),2] <- "soil_physics"
  transl_a[grep(transl_s[,2],pattern = "Silt"),2] <- "soil_physics"
  transl_a[grep(transl_s[,2],pattern = "Sand"),2] <- "soil_physics"
  transl_a[grep(transl_s[,2],pattern = "Clay"),2] <- "soil_physics"
  transl_a[grep(transl_s[,2],pattern = "Gravel"),2] <- "soil_physics"  
  
  transl_a[grep(transl_s[,2],pattern = "pH"),2] <- "soil_chemistry"
  transl_a[grep(transl_s[,2],pattern = "CEC"),2] <- "soil_chemistry"
  transl_a[grep(transl_s[,2],pattern = "C_content"),2] <- "soil_chemistry"
  transl_a[grep(transl_s[,2],pattern = "Cdensity"),2] <- "soil_chemistry"
  transl_a[grep(transl_s[,2],pattern = "C_stock"),2] <- "soil_chemistry"

  length(transl_a[!is.na(transl_a[,2]),2])==length(transl_s[!is.na(transl_s[,2]),2])

  #-----------
  # CLIMATE
  #-----------
  transl_c[which(transl_c[,1]%in%"wind.sd.nc"),2] <- "Wind_sd" 
  transl_c[which(transl_c[,1]%in%"wind.max.nc"),2] <- "Wind_max" 
  transl_c[which(transl_c[,1]%in%"wind.min.nc"),2] <- "Wind_min" 
  transl_c[which(transl_c[,1]%in%"wind.mean.nc"),2] <- "Wind" 
  transl_c[which(transl_c[,1]%in%"srad.sd.nc"),2] <- "Solar rad_sd" 
  transl_c[which(transl_c[,1]%in%"srad.max.nc"),2] <- "Solar rad_max" 
  transl_c[which(transl_c[,1]%in%"srad.min.nc"),2] <- "Solar rad_min" 
  transl_c[which(transl_c[,1]%in%"srad.mean.nc"),2] <- "Solar rad" 
  transl_c[which(transl_c[,1]%in%"vapr.sd.nc"),2] <- "Vapour pressure_sd" 
  transl_c[which(transl_c[,1]%in%"vapr.max.nc"),2] <- "Vapour pressure_max" 
  transl_c[which(transl_c[,1]%in%"vapr.min.nc"),2] <- "Vapour pressure_min" 
  transl_c[which(transl_c[,1]%in%"vapr.mean.nc"),2] <- "Vapour pressure" 
  transl_c[which(transl_c[,1]%in%"PrecipitationSeasonality.nc"),2] <- "Precipitation_sd" 
  transl_c[which(transl_c[,1]%in%"PrecipitationofDriestMonth.nc"),2] <- "Precipitation_min" 
  transl_c[which(transl_c[,1]%in%"PrecipitationofWettestMonth.nc"),2] <- "Precipitation_max" 
  transl_c[which(transl_c[,1]%in%"AnnualPrecipitation.nc"),2] <- "Precipitation" 
  transl_c[which(transl_c[,1]%in%"MeanDiurnalRange.nc"),2] <- "Temp_d" 
  transl_c[which(transl_c[,1]%in%"TemperatureSeasonality.nc"),2] <- "Temp_sd" 
  transl_c[which(transl_c[,1]%in%"MinTemperatureofColdestMonth.nc"),2] <- "Temp_min" 
  transl_c[which(transl_c[,1]%in%"MaxTemperatureofWarmestMonth.nc"),2] <- "Temp_max" 
  transl_c[which(transl_c[,1]%in%"AnnualMeanTemperature.nc"),2] <- "Temp" 

  #------------------
  # climate sd
  #------------------
  transl_c_sd[which(transl_c_sd[,1]%in%"PCA_sd_CLIMATE"),2] <- "climate_sd"
  transl_s_sd[which(transl_s_sd[,1]%in%"PCA_sd_SOIL"),2] <-  "soil_sd"
    
  #------------------
  #additions
  #------------------
  # transl_a[which(transl_a[,1]%in%"srad.sd.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"srad.max.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"srad.min.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"srad.mean.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"MeanDiurnalRange.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"TemperatureSeasonality.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"MinTemperatureofColdestMonth.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"MaxTemperatureofWarmestMonth.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"AnnualMeanTemperature.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"wind.sd.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"wind.max.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"wind.min.nc"),2] <- "energy" 
  # transl_a[which(transl_a[,1]%in%"wind.mean.nc"),2] <- "energy" 
  # 
  # transl_a[which(transl_a[,1]%in%"vapr.sd.nc"),2] <- "climate_water" 
  # transl_a[which(transl_a[,1]%in%"vapr.max.nc"),2] <- "climate_water" 
  # transl_a[which(transl_a[,1]%in%"vapr.min.nc"),2] <- "climate_water" 
  # transl_a[which(transl_a[,1]%in%"vapr.mean.nc"),2] <- "climate_water" 
  # transl_a[which(transl_a[,1]%in%"PrecipitationSeasonality.nc"),2] <- "climate_water" 
  # transl_a[which(transl_a[,1]%in%"PrecipitationofDriestMonth.nc"),2] <- "climate_water" 
  # transl_a[which(transl_a[,1]%in%"PrecipitationofWettestMonth.nc"),2] <- "climate_water" 
  # transl_a[which(transl_a[,1]%in%"AnnualPrecipitation.nc"),2] <- "climate_water" 
  
  
  transl_a[which(transl_a[,1]%in%"srad.sd.nc"),2] <- "variability" 
  transl_a[which(transl_a[,1]%in%"srad.max.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"srad.min.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"srad.mean.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"MeanDiurnalRange.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"TemperatureSeasonality.nc"),2] <- "variability" 
  transl_a[which(transl_a[,1]%in%"MinTemperatureofColdestMonth.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"MaxTemperatureofWarmestMonth.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"AnnualMeanTemperature.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"wind.sd.nc"),2] <- "variability" 
  transl_a[which(transl_a[,1]%in%"wind.max.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"wind.min.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"wind.mean.nc"),2] <- "minmaxmean" 
  
  transl_a[which(transl_a[,1]%in%"vapr.sd.nc"),2] <- "variability" 
  transl_a[which(transl_a[,1]%in%"vapr.max.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"vapr.min.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"vapr.mean.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"PrecipitationSeasonality.nc"),2] <- "variability" 
  transl_a[which(transl_a[,1]%in%"PrecipitationofDriestMonth.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"PrecipitationofWettestMonth.nc"),2] <- "minmaxmean" 
  transl_a[which(transl_a[,1]%in%"AnnualPrecipitation.nc"),2] <- "minmaxmean" 
  #----------------------------------------------------
  
  transl_a1[which(transl_a1[,1]%in%"srad.sd.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"srad.max.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"srad.min.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"srad.mean.nc"),2] <- "mean" 
  transl_a1[which(transl_a1[,1]%in%"MeanDiurnalRange.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"TemperatureSeasonality.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"MinTemperatureofColdestMonth.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"MaxTemperatureofWarmestMonth.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"AnnualMeanTemperature.nc"),2] <- "mean" 
  transl_a1[which(transl_a1[,1]%in%"wind.sd.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"wind.max.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"wind.min.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"wind.mean.nc"),2] <- "mean" 
  
  transl_a1[which(transl_a1[,1]%in%"vapr.sd.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"vapr.max.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"vapr.min.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"vapr.mean.nc"),2] <- "mean" 
  transl_a1[which(transl_a1[,1]%in%"PrecipitationSeasonality.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"PrecipitationofDriestMonth.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"PrecipitationofWettestMonth.nc"),2] <- "varouter" 
  transl_a1[which(transl_a1[,1]%in%"AnnualPrecipitation.nc"),2] <- "mean" 
  #----------------------------------------------------
  #alternative:
  transl_a2[which(transl_a2[,1]%in%"srad.sd.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"srad.max.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"srad.min.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"srad.mean.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"MeanDiurnalRange.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"TemperatureSeasonality.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"MinTemperatureofColdestMonth.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"MaxTemperatureofWarmestMonth.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"AnnualMeanTemperature.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"wind.sd.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"wind.max.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"wind.min.nc"),2] <- "climate_energy" 
  transl_a2[which(transl_a2[,1]%in%"wind.mean.nc"),2] <- "climate_energy" 
  
  transl_a2[which(transl_a2[,1]%in%"vapr.sd.nc"),2] <- "climate_water" 
  transl_a2[which(transl_a2[,1]%in%"vapr.max.nc"),2] <- "climate_water" 
  transl_a2[which(transl_a2[,1]%in%"vapr.min.nc"),2] <- "climate_water" 
  transl_a2[which(transl_a2[,1]%in%"vapr.mean.nc"),2] <- "climate_water" 
  transl_a2[which(transl_a2[,1]%in%"PrecipitationSeasonality.nc"),2] <- "climate_water" 
  transl_a2[which(transl_a2[,1]%in%"PrecipitationofDriestMonth.nc"),2] <- "climate_water" 
  transl_a2[which(transl_a2[,1]%in%"PrecipitationofWettestMonth.nc"),2] <- "climate_water" 
  transl_a2[which(transl_a2[,1]%in%"AnnualPrecipitation.nc"),2] <- "climate_water" 
  #----------------------------------------------------
  #alternative:
  transl_a3[which(transl_a3[,1]%in%"srad.sd.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"srad.min.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"MinTemperatureofColdestMonth.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"AnnualMeanTemperature.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"vapr.max.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"vapr.min.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"vapr.mean.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"PrecipitationofWettestMonth.nc"),2] <- "climate_A1" 
  transl_a3[which(transl_a3[,1]%in%"AnnualPrecipitation.nc"),2] <- "climate_A1" 
  
  transl_a3[which(transl_a3[,1]%in%"srad.max.nc"),2] <- "climate_A2" 
  transl_a3[which(transl_a3[,1]%in%"srad.mean.nc"),2] <- "climate_A2" 
  transl_a3[which(transl_a3[,1]%in%"MeanDiurnalRange.nc"),2] <- "climate_A2" 
  transl_a3[which(transl_a3[,1]%in%"TemperatureSeasonality.nc"),2] <- "climate_A2" 
  transl_a3[which(transl_a3[,1]%in%"MaxTemperatureofWarmestMonth.nc"),2] <- "climate_A2" 
  transl_a3[which(transl_a3[,1]%in%"PrecipitationSeasonality.nc"),2] <- "climate_A2" 
  transl_a3[which(transl_a3[,1]%in%"PrecipitationofDriestMonth.nc"),2] <- "climate_A2" 
  
  transl_a3[which(transl_a3[,1]%in%"wind.sd.nc"),2] <- "climate_A3" 
  transl_a3[which(transl_a3[,1]%in%"wind.max.nc"),2] <- "climate_A3" 
  transl_a3[which(transl_a3[,1]%in%"wind.min.nc"),2] <- "climate_A3" 
  transl_a3[which(transl_a3[,1]%in%"wind.mean.nc"),2] <- "climate_A3" 
  transl_a3[which(transl_a3[,1]%in%"vapr.sd.nc"),2] <- "climate_A3" 
  

  #--------------------------------------------------------------------------------------
  transl_t_sd[grep(transl_t_sd[,1],pattern = "sd.unique_spec"),2] <- "trait_sd" 
  
  vec <- rep("other",nrow(transl_s));
  vec[!is.na(transl_c[,2])] <- "climate"
  vec[!is.na(transl_s[,2])] <- "soil"
  vec[!is.na(transl_t[,2])] <- "trait"
  vec[!is.na(transl_t_sd[,2])] <- "trait_sd"
  vec[!is.na(transl_s_sd[,2])] <- "soil_sd"
  vec[!is.na(transl_c_sd[,2])] <- "climate_sd"
  vec[!is.na(transl_b[,2])] <- "biome"
  
  vec2 <- rep("other",nrow(transl_s));
  vec2[!is.na(transl_c[,2])] <- transl_c[!is.na(transl_c[,2]),2]
  vec2[!is.na(transl_s[,2])] <- transl_s[!is.na(transl_s[,2]),2]
  vec2[!is.na(transl_t[,2])] <- transl_t[!is.na(transl_t[,2]),2]
  vec2[!is.na(transl_b[,2])] <- transl_b[!is.na(transl_b[,2]),2]
  
  vec3 <- rep("other",nrow(transl_s));
  vec3[!is.na(transl_a[,2])] <- transl_a[!is.na(transl_a[,2]),2]
  
  vec4 <- rep("other",nrow(transl_d));
  vec4[!is.na(transl_d[,2])] <- transl_d[!is.na(transl_d[,2]),2]

  vec5 <- rep("other",nrow(transl_a1));
  vec5[!is.na(transl_a1[,2])] <- transl_a1[!is.na(transl_a1[,2]),2]
  vec6 <- rep("other",nrow(transl_a2));
  vec6[!is.na(transl_a2[,2])] <- transl_a2[!is.na(transl_a2[,2]),2]
  vec7 <- rep("other",nrow(transl_a3));
  vec7[!is.na(transl_a3[,2])] <- transl_a3[!is.na(transl_a2[,2]),2]
  
  out <- cbind(input,vec,vec2,vec3,vec4,vec5,vec6,vec7)
  
#  out <- list(climate = transl_c,
#              soil=transl_s,
#              traits=transl_t)
 
  return(out)
}

