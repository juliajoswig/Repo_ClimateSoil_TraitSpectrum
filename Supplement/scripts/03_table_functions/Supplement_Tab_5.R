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

  res <- matrix(NA,ncol=5,nrow=17)
  colnames(res) <- c("Trait names","Trait Group", "Trait name according to TRY / TraitID",	"Unit",	"nb of observations")
  trait_names1 <- c("Leaf carbon (C) content per leaf dry mass",
               "Leaf nitrogen (N) content per leaf dry mass",
               "Leaf nitrogen (N) content per leaf area ",
               "Leaf nitrogen/phosphorus (N/P) ratio",
               "Leaf phosphorus (P) content per leaf dry mass",
               "Leaf area per leaf dry mass (specific leaf are, SLA)",
               "Stem dry mass per stem fresh volume (stem specific density, SSD, wood density)",
               "Leaf nitrogen (N) isotope signature (delta 15N)",
               "Seed number per reproduction unit",
               "Wood vessel element length",
               "Stem conduit density (vessels and tracheids)",
               "Dispersal unit length",
               "Leaf area",
               "Leaf fresh mass",
               "Plant height ",
               "Seed dry mass",
               "Seed length")
  trait_names2 <- c("LeC","LeN","LeNArea","LeNP","LeP","SLA","SSD","Led15N","SenbU","VesLen","ConduitDens",
                   "DispULen","LeArea","LeFMass","PlantHeight","SeedMass","SeLen")
  trait_id <- c(13,14,50,56,15,11,4,78,138,282,169,237,1,163,18,26,27)
  
  res[,1] <- Rename_Vars(trait_names2)[,3]
  res[,2] <- put_into_traitGroup(trait_names2)
  res[,3] <- paste0(trait_names1,"/ ",trait_id)
  res[,4] <- c("mg g-1","mg g-1","g m-2","g g-1","mg g-1","mm2 mg-1",
               "mg mm-3","???","","??m","mm-2","cm","mm2", "mg","m","mg","mm")
  for(i in 1:17){
  res[i,5] <- sum(TRY_Env$info[[paste0("observation.count.",trait_names2[i])]])
  }
  res <- data.frame(res)
  res <- res[order(res[,2]),]
  
  if(!file.exists(file.path(origin, "tables"))){
    dir.create(file.path(origin, "tables"))}
  if(!file.exists(file.path(origin, "tables","Supplement_Tab_5"))){
    dir.create(file.path(origin, "tables","Supplement_Tab_5"))}
  
  #install.packages("xlsx")
#  require("xlsx")
#  write.xlsx(res, file=file.path(origin,"data","Results","tables","Supplement_Tab_5","Table_S2.xls"), sheetName="Sheet1", 
#             col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)
  write.csv(res, file=file.path(origin, "tables","Supplement_Tab_5","Supplement_Tab_5.csv"))
  print(xtable(res, type = "latex"), file =file.path(origin, "tables","Supplement_Tab_5","Supplement_Tab_5.tex"))
  print(xtable(res, type = "latex"),include.rownames=FALSE)
  
