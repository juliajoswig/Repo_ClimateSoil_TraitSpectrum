
#----------------------------------------
# get data agg1
#-----------------------------------------
list.files(file.path(origin,"data","master_matrix","_aggregated_agg1","NA_mnNbr"))
list.files(file.path(origin,"data","master_matrix","_aggregated_agg0","TRY","NA_mnNbr"))
load(file = file.path(origin,"data","master_matrix","_aggregated_agg1","NA_mnNbr","TRY_Env1_20191111.RData"))
ECO_ID 

TRY_sel <- TRY_Env1[TRY_Env1$ECO_ID %in% ECO_ID,]

length(unique(TRY_sel$Species_gf))

load(file = file.path(origin,"data","master_matrix","_aggregated_agg0","TRY","NA_mnNbr","TRY_Env_20191111.RData"))

# nb of locations
TRY_sel0 <- TRY_Env[TRY_Env$ECO_ID %in% ECO_ID,]
length(unique(paste0(TRY_Env$Lat,TRY_Env$Lon)))
length(unique(paste0(TRY_sel0$Lat,TRY_sel0$Lon)))
names(TRY_Env)
length(unique(paste0(TRY_Env[,133:271])))
length(unique(paste0(TRY_sel0[,133:271])))
