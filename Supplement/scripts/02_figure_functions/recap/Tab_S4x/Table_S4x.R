

setwd("/Net/Groups")
orig_loctem =  "BGI"#"/Volumes/BGI" 
#orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
output_term="_sel20_"#_minmaxmed # "sel20SoilExtend_"
spec_count=20 # if output_term == "_sel20_"
NA_now="NA_mnNbr"#"NA_stays_NA"

# ---------------------------------------------------------------------------------------
# define the origin path
# ---------------------------------------------------------------------------------------

# origin = # please add your local path here & comment the ones below.
origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")

require("dplyr")
# get some functions
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_Rename_Vars3.R"))
if(output_term=="sel20SoilExtend_"){source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_Rename_Vars_extendedSoilData2.R"))}
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_put_into_traitGroup.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_traits.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_biomes.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_add_alpha.R"))
source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","pl_plot_overlaySqunb.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","001a_prep_TRY_Env.R"))

#source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","fig_4","plot_Figure_4_20191108.R"))
#source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","fig_2","plot_Figure_2_20191108.R")) #lat gradients
#source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","fig_S1","plot_Figure_S1_20191108.R")) #PCA


if(1!=1){ plot_Figure_1(origin) } # tbd
if(1!=1){ plot_Figure_2(origin) }

# ---------------------------------------------------------------------------------------
# load data
# ---------------------------------------------------------------------------------------
# For Ecoregion - Scale:
load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
                      "_aggregated_agg2",NA_now,"TRY_Env2_Kier_20191111.RData"))
# For Grid - Scale:
#load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
#                    "_aggregated_agg2",NA_now,"TRY_Env2grid_Kier_20191111.RData"))
# For Coordinate - Scale:
#load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
#                      "_aggregated_agg2",NA_now,"TRY_Env2coord_Kier_20191111.RData"))

TRY_Env_o <- TRY_Env2_Kier

# ---------------------------------------------------------------------------------------
# process data:
# ---------------------------------------------------------------------------------------
# Define data set
Appr_types = c("Data_GapFilled","Data_Predicted","Data_Observed")
at=1
Appr_type_now=Appr_types[at]
#prep data:
out <- prep_TRY_Env(Appr_type_now,output_term,Rename_Vars,TRY_Env_o,spec_count)

data_master_2=out$data_master_2
info_2=out$info_2
trait_2=out$trait_2
soil_2=out$soil_2
climate_2=out$climate_2
noise_2=out$noise_2
climatewater_2=out$climatewater_2
wind_2=out$wind_2
soilwater_2=out$soilwater_2
soiltopo_2=out$soiltopo_2
energy_2=out$energy_2
nutrients_2=out$nutrients_2

#------------------------------------------------------------
##  Hierarchical Partitioning
#------------------------------------------------------------
list.files(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression"))
#source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegression20191111.R"))
source(file.path(origin,"scripts",Appr_types[at],"04_plot_scripts","fig_3","plot_Figure_3_20191204.R"))
# define data set to be loaded
nruns=50
folds=10
ncomp_PCA=20
do_PCA=TRUE
output_term="_sel20_"

# load analysis Ridge Regression N2
sel_now="soilAclimate"
#sel_now="nutrientsAsoilwater"
#sel_now="energyAclimatewater"
#sel_now="nutrientsAenergy"
sel_now="soilwaterAclimatewater"
#sel_now="noiseAclimate"
#sel_now="soilAnoise"
#sel_now="nutrientsAclimate"
#sel_now="soiltopoAclimate"
#sel_now="soilAclimatewater"
#sel_now="soilAenergy"
#sel_now="soilAwind"
#sel_now="noiseAclimate"
#sel_now="noiseAsoil"
#sel_now="nutrientsAclimate"
#sel_now="soiltopoAclimate"
#sel_now="soilwaterAclimate"
source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegressionEASY_20191203.R"))
out_N2 <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,firstname,secondname,nruns,output_term,ncomp_PCA,do_PCA)  
source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","hierarchical_partitioning","hierarchical_partitioning20191204.R"))
hp_now <- hierarchical_partitioning(origin,out_now=out_N2,output_term,oldData,out_rf,model_now="Ridge_Regression",trait_2,sel_now)
out_now=out_N2
Figure_3b(out_now,hp_now,origin,sel_now)
#hp_nowsc <- hp_now
#hp_now4 <- hp_now
#hp_now1 <- hp_now
#hp_now2 <- hp_now
#hp_now3 <- hp_now
# likert plot  

# load analysis Ridge Regression N3
source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegressionEASYN3_20191213.R"))
  sel_now="soilwaterAAnutrientsAsoiltopo"
  sel_now="soilAAnutrientsAsoiltopo"
  sel_now="soilAAsoilwaterAsoiltopo"
  sel_now="soilAAnutrientsAsoilwater"
  #sel_now="energyAAwindAclimatewater"
out_N3 <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,sel_now,nruns,output_term,ncomp_PCA,do_PCA)  

source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","hierarchical_partitioning","hierarchical_partitioning_N3_20191213.R"))

N3_res <- matrix(NA,ncol=3,nrow=17)
rownames(N3_res) <- colnames(out_N2[[1]])
N3_res2 <- N3_res
N3_res_V2 <- N3_res
colnames(N3_res) <- c(names(out_N3)[1:3])

i=1
for(i in 1:17){
  X0=0
  X1=mean(out_N3[[1]][,i])
  X2=mean(out_N3[[2]][,i])
  X3=mean(out_N3[[3]][,i])
  X12=mean(out_N3[[4]][,i])
  X13=mean(out_N3[[5]][,i])
  X23=mean(out_N3[[6]][,i])
  if(sel_now=="energyAAwindAclimatewater"){X123=mean(out_N2$r2_climate[,i])}
  if(sel_now=="soilwaterAAnutrientsAsoiltopo"){X123=mean(out_N2$r2_soil[,i])}
  hp <- hierarchical_partitioning_N3(X0,X1,X2,X3,X12,X13,X23,X123)
  N3_res[i,] <- c(hp[[1]],hp[[2]],hp[[3]])*100
  N3_res2[i,] <- c(hp[[1]]-hp[[7]],hp[[2]]-hp[[7]],hp[[3]]-hp[[9]])*100
  N3_res_V2[i,] <- c(mean(out_N2[[2]][,i]-out_N3[[2]][,i]-out_N3[[3]][,i]),
                     mean(out_N2[[2]][,i]-out_N3[[1]][,i]-out_N3[[3]][,i]),
                     mean(out_N2[[2]][,i]-out_N3[[1]][,i]-out_N3[[2]][,i]))*100
}

N3_res_V2[N3_res_V2<0]=0
N3_res2[N3_res2<0]=0
N3_res[N3_res<0]=0

print()
N3_res
N3_res2
N3_res_V2

colMeans(N3_res)


require("xlsx")
write.xlsx(res2, file=file.path(origin,"data","Results","tables","Tab_S5","Table_S5b.xls"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)



#---------------------
#  plot(hp_now1$indep_nutrients, hp_nowsc$indep_soil)
#  abline(0,1,col="red")
#  plot(hp_now3$indep_soilwater, hp_nowsc$indep_soil)
#  abline(0,1,col="red")
#plot(hp_now2$indep_soiltopo, hp_nowsc$indep_soil)
#abline(0,1,col="red")
par(mfrow=c(1,4),mar=c(0,0,0,0))
a=(hp_now1[[1]]-hp_now1[[3]]);a[a<0]=0
b=(hp_now1[[2]]-hp_now1[[4]]);b[b<0]=0
plot(colMeans(a),colMeans(b),pch=16,xlim=c(0,1),ylim=c(0,1),
     xlab="nutrients (indep fraction of topoAclimateRun)", ylab="climate(indep fraction of soilAclimateRun)",col=color_to_traits(colnames(hp_now1$indep_nutrients)))
a=(hp_now3[[1]]-hp_now3[[3]]);a[a<0]=0
b=(hp_now3[[2]]-hp_now3[[4]]);b[b<0]=0
plot(colMeans(a),colMeans(b),pch=16,xlim=c(0,1),ylim=c(0,1),
     xlab="soilwater (indep fraction of topoAclimateRun)", ylab="climate(indep fraction of soilAclimateRun)",col=color_to_traits(colnames(hp_now1$indep_nutrients)))
a=(hp_now2[[1]]-hp_now2[[3]]);a[a<0]=0
b=(hp_now2[[2]]-hp_now2[[4]]);b[b<0]=0
plot(colMeans(a),colMeans(b),pch=16,xlim=c(0,1),ylim=c(0,1),
     xlab="soiltopo (indep fraction of topoAclimateRun)", ylab="climate(indep fraction of soilAclimateRun)",col=color_to_traits(colnames(hp_now1$indep_nutrients)))
a=(hp_nowsc[[1]]-hp_nowsc[[3]]);a[a<0]=0
b=(hp_nowsc[[2]]-hp_nowsc[[4]]);b[b<0]=0
plot(colMeans(a),colMeans(b),pch=16,xlim=c(0,1),ylim=c(0,1),
     xlab="soil (indep fraction of topoAclimateRun)", ylab="climate(indep fraction of soilAclimateRun)",col=color_to_traits(colnames(hp_now1$indep_nutrients)))
# do likert plot
par(mfrow=c(1,3))
a=(hp_now1[[2]]-hp_now1$joint_climate);a[a<0]=0
b=(hp_nowsc$indep_climate-hp_nowsc$joint_climate);b[b<0]=0
boxplot(a-b,las=2,ylab="nutrients (indep fraction of topoAclimateRun) - soil(indep fraction of soilAclimateRun)",col=color_to_traits(colnames(hp_now1$indep_nutrients)))
abline(0,0,col="red")
a=(hp_now3[[2]]-hp_now3$joint_climate);a[a<0]=0
b=(hp_nowsc$indep_climate-hp_nowsc$joint_climate);b[b<0]=0
boxplot(a-b,ylim=c(-.3,.3),las=2,ylab="soilwater (indep fraction of topoAclimateRun) - soil(indep fraction of soilAclimateRun)",col=color_to_traits(colnames(hp_now1$indep_nutrients)))
abline(0,0,col="red")
a=(hp_now2[[2]]-hp_now2$joint_climate);a[a<0]=0
b=(hp_nowsc$indep_climate-hp_nowsc$joint_climate);b[b<0]=0
boxplot(a-b,ylim=c(-.3,.3),las=2,ylab="soiltopo (indep fraction of topoAclimateRun) - soil(indep fraction of soilAclimateRun)",col=color_to_traits(colnames(hp_now1$indep_nutrients)))
abline(0,0,col="red")

par(mfrow=c(1,2),mar=c(4,4,2,2))
plot(colMeans(hp_now1[[1]]-hp_now1$joint_nutrients),colMeans(hp_now1[[2]]-hp_now1$joint_nutrients),ylim=c(0,1),las=2,pch=16,col=color_to_traits(colnames(hp_now1$indep_nutrients)))
text(colMeans(hp_now1[[1]]-hp_now1$joint_nutrients),colMeans(hp_now1[[2]]-hp_now1$joint_nutrients),ylim=c(0,1),las=2,pch=16,col=color_to_traits(colnames(hp_now1$indep_nutrients)),labels=colnames(hp_now1$indep_nutrients))
plot(colMeans(hp_nowsc$indep_soil-hp_nowsc$joint_soil),colMeans(hp_nowsc$indep_climate-hp_nowsc$joint_climate),ylim=c(0,1),las=2,pch=16,col=color_to_traits(colnames(hp_now1$indep_nutrients)))
text(colMeans(hp_nowsc$indep_soil-hp_nowsc$joint_soil),colMeans(hp_nowsc$indep_climate-hp_nowsc$joint_climate),ylim=c(0,1),las=2,pch=16,col=color_to_traits(colnames(hp_now1$indep_nutrients)),labels=colnames(hp_now1$indep_nutrients))

par(mfrow=c(1,2))
plot(colMeans(hp_now1$indep_nutrients-hp_now1$joint_nutrients),colMeans(hp_now1$indep_climate-hp_now1$joint_nutrients),ylim=c(0,1),las=2,pch=16,col=color_to_traits(colnames(hp_now1$indep_nutrients)))
plot(colMeans(hp_now$indep_soil-hp_now$joint_soil),colMeans(hp_now$indep_climate-hp_now$joint_climate),ylim=c(0,1),las=2,pch=16,col=color_to_traits(colnames(hp_now1$indep_nutrients)))


boxplot(hp_now4$indep_noise/hp_now4$indep_climate-
          hp_now$indep_soil/hp_now$indep_climate,ylab="indep noise-indep soil",ylim=c(-.4,.4),las=2)
abline(0,0,col="red")
boxplot(hp_now1$indep_nutrients/hp_now1$indep_climate-
          hp_nowsc$indep_soil/hp_nowsc$indep_climate,ylim=c(-.4,.4),las=2,ylab="ratio: nutrients/climate(indep+joint) - ratio soil/climate(indep+joint)",
        col=color_to_traits(colnames(hp_now[[1]])))
abline(0,0,col="red")
boxplot(hp_now3$indep_soilwater/hp_now3$indep_climate-
          hp_nowsc$indep_soil/hp_nowsc$indep_climate,ylim=c(-.3,.3),las=2,ylab="ratio: soilwater/climate(indep+joint) - ratio soil/climate(indep+joint)")
abline(0,0,col="red")
boxplot(hp_now2$indep_soiltopo/hp_now2$indep_climate-
          hp_nowsc$indep_soil/hp_nowsc$indep_climate,ylim=c(-.3,.3),las=2,ylab="ratio: soiltopo/climate(indep+joint) - ratio soil/climate(indep+joint)")
abline(0,0,col="red")
