
Setwd("/Net/Groups")
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
trait_names <- c("LeArea","Led15N","LeNArea","LeNP","LeC","LeP","LeN","SLA","ConduitDens",
                 "LeFMass","SenbU","PlantHeight","SeLen","VesLen","DispULen","SeedMass","SSD")

# ---------------------------------------------------------------------------------------
# load analysis Ridge Regression
# ---------------------------------------------------------------------------------------
nruns=10
folds=10
source(file.path(origin,"scripts","Data_GapFilled","2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegression.R"))
out_RR <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,nruns,output_term="OLM_data")  
plot(colMeans(out_RR$r2_climate[,1:17]),colMeans(out_RR$r2_climate[,18:34]),las=3,col="white")
text(colMeans(out_RR$r2_climate[,1:17]),colMeans(out_RR$r2_climate[,18:34]),las=3,labels = colnames(out_RR$r2_soil)[1:17])
abline(0,1)
plot(colMeans(out_RR$r2_soil[,1:17]),colMeans(out_RR$r2_soil[,18:34]),las=3,col="white")
text(colMeans(out_RR$r2_soil[,1:17]),colMeans(out_RR$r2_soil[,18:34]),las=3,labels = colnames(out_RR$r2_soil)[1:17])
abline(0,1)

names(trait_2)
i=3
par(mfrow=c(1,2))
hist(trait_2[,i])
hist(log(trait_2[,i]))

# ---------------------------------------------------------------------------------------
# load analysis Random Forest
# ---------------------------------------------------------------------------------------
dev.off()
nruns=1
folds=2
source(file.path(origin,"scripts","Data_GapFilled","2_analysis_function","random_forest","fn_load_analysis_random_forest.R"))
out_rf <- load_analysis_random_forest(origin,nruns,folds,Appr_type_now,nruns,output_term)  
out_rf
plot(out_rf$r2_soil,out_rf$r2_climate,col=color_to_traits(colnames(out_rf$r2_soil)),pch=16,xlim=c(0,1),ylim=c(0,1))
text(out_rf$r2_soil,out_rf$r2_climate,col=color_to_traits(colnames(out_rf$r2_soil)),labels = colnames(out_rf$r2_soil))

plot(out_rf$r2_noise,out_rf$r2_climate,col=color_to_traits(colnames(out_rf$r2_soil)),pch=16,xlim=c(0,1),ylim=c(0,1))
text(out_rf$r2_noise,out_rf$r2_climate,col=color_to_traits(colnames(out_rf$r2_soil)),labels = colnames(out_rf$r2_soil))
