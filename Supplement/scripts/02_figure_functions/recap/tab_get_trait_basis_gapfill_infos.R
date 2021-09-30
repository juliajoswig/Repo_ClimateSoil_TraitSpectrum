setwd("/Net/Groups")
# JJoswig 2019_10_18
orig_loctem =  "BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
#orig_loctem =  "/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
origin_Agg0data =file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix/_aggregated_agg0")

list.files(file.path(origin_Agg0data,"TRY","predicted","2015","Gap_Filled_Data_FF"))

#------------------------------------------------------------------------
# read some functions
#------------------------------------------------------------------------
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_Rename_Vars.R"))

list.files(file.path(origin_Agg0data,"TRY","predicted","2015"))
TRY_pred <- read.csv(file.path(origin_Agg0data,"TRY","predicted",
                                "2015","Gap_Filled_Data_FF","GapFilling2015_01_31_cleaned.csv"),header = T)
# TRY_geo <- read.csv(file.path(origin_Agg0data,"TRY","predicted",
#                               "2015","GapFilling2015_01_31_geo.csv"),header = T)
# load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix","_aggregated_agg2",NA_now,"TRY_Env2_Kier_20191022.RData"))
#TRY <- TRY_geo # OR
TRY <- TRY_pred 

dim(TRY)
names(TRY)
trait_names <- names(TRY)[15:ncol(TRY)]
  m <- matrix(NA,ncol=6,nrow=length(trait_names))  
  colnames(m) <- c("nb_observed","%observed","nb_species","Genera","PlantGrowthForm","median number of jointly measured")
  rownames(m) <- trait_names
  
  i=1
  for( i in 1:length(trait_names)){
    colm=which(names(TRY)%in%trait_names[i])
  m[i,1] <- sum(!is.na(TRY[,colm]))
  m[i,2] <- round(sum(!is.na(TRY[,colm]))/nrow(TRY)*100,digits=2)
#  m[i,3] <- length(unique(TRY$Species[!is.na(TRY[,colm])]))
#  m[i,4] <- length(unique(TRY$Genus[!is.na(TRY[,colm])]))
#  m[i,5] <- length(unique(TRY$PlantGrowthForm[!is.na(TRY[,colm])]))
  ix <- !is.na(TRY[,colm])
  m[i,6] <- median(rowSums(!is.na(TRY[ix,which(names(TRY)%in%trait_names)])))
  }
  rownames(m)[Rename_Vars(rownames(m))[,3]!="other"] <-  Rename_Vars(rownames(m))[,3][Rename_Vars(rownames(m))[,3]!="other"]
  m2 <- m[!(1:nrow(m)%in%grep(x = rownames(m),pattern = "X")),] 
  #write.csv(m,file=file.path(origin,"data","Results","tables","Trait_values_observed_geo.csv"))
  require("xlsx")
  write.xlsx(m2, file=file.path(origin,"data","Results","tables","Table_S6_GapFilling.xls"), sheetName="Sheet1", 
             col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)

  
  
  
  