# Data change
# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
setwd("/Net/Groups")
orig_loctem ="/Volumes/BGI" # local
orig_loctem =  "BGI"#"/Volumes/BGI" 
output_term = "" 
origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/Repo_ClimateSoil_TraitSpectrum/Supplement" 
origin_source = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/data_not_public/master_data/UNCUT_Supplement_data_20211129" 
# origin = # please add your local path here & comment the ones below.

list.files(file.path(origin,"scripts/_master"))


# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))# check, this does not exist.

list.files(file.path(origin,"data","master_matrix"))

# ---------------------------------------------------------------------------------------
# load data
# ---------------------------------------------------------------------------------------
load(file.path(origin,"data","master_matrix","X2__NEE.RData"))
soil <- TRY_Env$soil

  library(xtable)

  res <- matrix(NA,ncol=4,nrow=ncol(soil))
  colnames(res) <- c("Short form","File name","Description","Unit")
  res[,1] <- Rename_Vars(colnames(soil))[,3]
  res[,2]<- colnames(soil)
  res[grep(res[,2],pattern = "OCDENS"),3] <- "Soil organic carbon density"
  res[grep(res[,1],pattern = "C_density"),4]<- "kg m−3" 
  
  res[grep(res[,2],pattern = "PHIKCL"),3] <- "pH index measured in KCl solution, at 0.00 m"
  res[grep(res[,2],pattern = "PHIKCL"),4]<- "-log( H+ ions )"
  
  res[grep(res[,2],pattern = "OCSTHA"),3] <- "Soil organic carbon stock, at 0.00 m"
  res[grep(res[,2],pattern = "OCSTHA"),4] <- "t/ha"
  
  res[grep(res[,2],pattern = "ORCDRC"),3] <- "Soil organic carbon content, at 0.00 m"
  res[grep(res[,2],pattern = "ORCDRC"),4] <- "‰ or g / kg"
  
  res[grep(res[,2],pattern = "SLTPPT"),3] <- "Weight percentage of the silt particles (0.0002–0.05 mm), at 0.00 m"
  res[grep(res[,2],pattern = "SLTPPT"),4] <- "g 100 g-1"
  
  res[grep(res[,2],pattern = "SNDPPT"),3] <- "Weight percentage of the sand particles (0.05–2 mm), at 0.00 m"
  res[grep(res[,2],pattern = "SNDPPT"),4] <- "g 100 g-1"
  
  res[grep(res[,2],pattern = "CLYPPT"),3] <- "Weight percentage of the clay particles (<0.0002 mm), at 0.00 m"
  res[grep(res[,2],pattern = "CLYPPT"),4] <- "g 100 g-1"
  
  res[grep(res[,2],pattern = "CRFVOL"),3] <- "Volumetric percentage of coarse fragments (>2 mm), at 0.00 m"
  res[grep(res[,2],pattern = "CRFVOL"),4] <- "m3 100 m-3"
  
  res[grep(res[,2],pattern = "BLDFIE"),3] <- "Bulk density (fine earth)"
  res[grep(res[,2],pattern = "BLDFIE"),4] <- "kg m-3"
  
  
  res[grep(res[,2],pattern = "AWCtS"),3] <- "Saturated water content, at depth 0.00 m"
  res[grep(res[,2],pattern = "AWCtS"),4] <- "Vol%"
  
  
  res[grep(res[,2],pattern = "AWCh1"),3] <- "Available soil water capacity (volumetric fraction) with FC = pF 2.0, at 0.00 m"
  res[grep(res[,2],pattern = "AWCh1"),4] <- "Vol%"
  
  res[grep(res[,2],pattern = "AWCh2"),3] <- "Available soil water capacity for moisture potential (-20 kPa; pF2.3), at depth 0.00 m"
  res[grep(res[,2],pattern = "AWCh2"),4] <- "Vol%"
  
  res[grep(res[,2],pattern = "AWCh3"),3] <- "Available soil water capacity for moisture potential (-31.6 kPa; pF2.5), at depth 0.00 m at 0.00 m"
  res[grep(res[,2],pattern = "AWCh3"),4] <- "Vol%"
  
  res[grep(res[,2],pattern = "WWP"),3] <- "Available soil water capacity (volumetric fraction) until wilting point, at depth 0.00 m"
  res[grep(res[,2],pattern = "WWP"),4] <- "‰ or g / kg"
 
  res[grep(res[,2],pattern = "CECSOL"),3] <- "Cation exchange capacity of soil in cmolc/kg at depth 0.00 m"
  res[grep(res[,2],pattern = "CECSOL"),4] <- "cmolc/kg"
  
  
  
  res[grep(res[,2],pattern = "sl2"),3] <- "- at depth 0.05m"
  res[grep(res[,2],pattern = "sl3"),3] <- "- at depth 0.15m"
  res[grep(res[,2],pattern = "sl4"),3] <- "- at depth 0.30m"
  res[grep(res[,2],pattern = "sl5"),3] <- "- at depth 0.60m"
  res[grep(res[,2],pattern = "sl6"),3] <- "- at depth 1m"
  res[grep(res[,2],pattern = "sl7"),3] <- "- at depth 2m"
  res[grep(res[,2],pattern = "sd2"),3] <- "- at depth 0.05m"
  res[grep(res[,2],pattern = "sd3"),3] <- "- at depth 0.15m"
  res[grep(res[,2],pattern = "sd4"),3] <- "- at depth 0.30m"
  res[grep(res[,2],pattern = "sd5"),3] <- "- at depth 0.60m"
  res[grep(res[,2],pattern = "sd6"),3] <- "- at depth 1m"
  res[grep(res[,2],pattern = "sd7"),3] <- "- at depth 2m"
  res
#-----
    
  res <- data.frame(res)
  ix <- order(res[,1],decreasing = FALSE)
  res <- res[ix,]
  
  if(!file.exists(file.path(origin, "tables"))){
    dir.create(file.path(origin, "tables"))}
  if(!file.exists(file.path(origin, "tables","Supplement_Tab_2"))){
    dir.create(file.path(origin, "tables","Supplement_Tab_2"))}
  
  write.csv(res, file=file.path(origin, "tables","Supplement_Tab_2","Tab_S_SoilInfo.csv"))

  tableSb <- xtable(res, caption="A Very Long Table", label="ALongTable")
  print(tableSb, include.rownames=FALSE, tabular.environment="longtable", floating=FALSE)
  print(xtable(res, type = "latex"), file =file.path(origin,"tables","Supplement_Tab_2","Tab_S_SoilInfo.tex"))
