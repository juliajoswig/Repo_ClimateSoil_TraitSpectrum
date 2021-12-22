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

  library(xtable)
  trait_names <- names(TRY_Env$trait)
  
  Table_S6 <- cbind(paste0(TRY_Env$info$ECO_NAME,"/ ",TRY_Env$info$ECO_ID),
                    paste0(TRY_Env$info$species.count," (",TRY_Env$info$observation.count.tot,")"),
                    round(TRY_Env$info$Kier_richness,digits = 0),
                    paste0(round(TRY_Env$info$Lat,digits = 1)," (",round(TRY_Env$info$min.lat,digits = 0),";",round(TRY_Env$info$max.lat,digits = 0),")"),
                    paste0(round(TRY_Env$info$Lon,digits = 1)," (",round(TRY_Env$info$min.lon,digits = 0),";",round(TRY_Env$info$max.lon,digits = 0),")"),
                    round(TRY_Env$info$Kier_area,digits = 0))
  
  colnames(Table_S6) <- c("Name of the ecoregion / ID",
                          "Number of species (and observations)",
                          "Plant Richness",
                          "Lat (min; max)",
                          "Lon (min; max)",
                          "Area")
  
  TRY_Env$info$species.count[order(TRY_Env$info$species.count,decreasing = T)]
  Table_S6 <- Table_S6[order(TRY_Env$info$species.count,decreasing = T),]
  head(Table_S6)

  if(!file.exists(file.path(origin, "tables"))){
    dir.create(file.path(origin, "tables"))}
  if(!file.exists(file.path(origin, "tables","Supplement_Tab_6"))){
    dir.create(file.path(origin, "tables","Supplement_Tab_6"))}
  
  
  library(xtable)
  
  write.csv(Table_S6, file= file.path(origin,"tables","Supplement_Tab_6","Tab_S_ER.csv"))
  tableSb <- xtable(Table_S6, caption="A Very Long Table", label="ALongTable")
  print(tableSb, include.rownames=FALSE, tabular.environment="longtable", floating=FALSE)
  print(xtable(Table_S6, type = "latex"), file =file.path(origin,"tables","Supplement_Tab_6","Tab_S_ER.tex"))

