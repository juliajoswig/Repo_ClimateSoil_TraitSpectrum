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

climate <- TRY_Env$climate
  library(xtable)

  res <- matrix(NA,ncol=4,nrow=21)
  colnames(res) <- c("Short form","File name","Description","Unit")
  res[,1] <- Rename_Vars(colnames(climate))[,3]
  res[,2]<- colnames(climate)
  res[,3]<- c("Annual mean temperature" ,
              "Maximal temperature of warmest month",
              "Minimum temperature of coldest month", 
              "Temperature seasonality" ,
              "Mean diurnal temperature range", 
              "Total annual precipitation",
              "Total precipitation of wettest month",
              "Total precipitation of dryest month",
              "Annual sd of monthly sum of precipitation",
              "Mean annual water vapor pressure (kPa)",
              "Maximum annual water vapor pressure (kPa)", 
              "Minimum annual water vapor pressure (kPa)",
              "Standard deviation of water vapor pressure (kPa)",
              "Annual average of monthly mean solar radiation",
              "Mean solar radiation of brightest month",
              "Mean solar radiation of darkest month",
              "Annual sd of monthly mean solar radiation",
              "Average monthly wind speed",
              "Average wind speed of windiest month",
              "Average wind speed of most windless month",
              "Annual sd of monthly wind speed")
  #res[,4] <- c("mm","mm","mm","mm","kJ m-2 day-1","kJ m-2 day-1","kJ m-2 day-1","kJ m-2 day-1",
  #             "°C","°C","°C","°C","°C","kPa","kPa","kPa","kPa","M s-1","M s-1","M s-1","M s-1")
  res[grep(res[,1],pattern = "Precipitation"),4]<- "mm"
  res[grep(res[,1],pattern = "Temp"),4]<- "°C"
  res[grep(res[,1],pattern = "Vapour"),4]<- "kPa"
  res[grep(res[,1],pattern = "Solar"),4]<- "kJ m-2 day-1"
  res[grep(res[,1],pattern = "Wind"),4]<- "M s-1"

  res <- data.frame(res)
  ix <- order(res[,1],decreasing = FALSE)
  res <- res[ix,]
  
  if(!file.exists(file.path(origin, "tables"))){
    dir.create(file.path(origin, "tables"))}
  if(!file.exists(file.path(origin, "tables","Supplement_Tab_1"))){
    dir.create(file.path(origin, "tables","Supplement_Tab_1"))}
  
  write.csv(res, file=file.path(origin, "tables","Supplement_Tab_1","Tab_S_ClimateInfo.csv"))
  print(xtable(res, type = "latex"), file =file.path(origin, "tables","Supplement_Tab_1","Tab_S_ClimateInfo.tex"))
  print(xtable(res, type = "latex"),include.rownames=FALSE)
  
