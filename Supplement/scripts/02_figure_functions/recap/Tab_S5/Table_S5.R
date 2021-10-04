

### TERMINAL:
setwd("/Net/Groups")
orig_loctem =  "BGI"#"/Volumes/BGI" 
#orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
output_term="_sel20_"#_minmaxmed
spec_count=20 # if output_term == "_sel20_"
NA_now="NA_mnNbr"#"NA_stays_NA"

# ---------------------------------------------------------------------------------------
# define the origin path
# ---------------------------------------------------------------------------------------

# origin = # please add your local path here & comment the ones below.
origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")

# get some functions
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_Rename_Vars3.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_put_into_traitGroup.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_traits.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_biomes.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_add_alpha.R"))
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","001a_prep_TRY_Env.R"))
source(file.path(origin,"scripts","Data_GapFilled","2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegressionEASY_20191203.R"))
source(file.path(origin,"scripts","Data_GapFilled","2_analysis_function","hierarchical_partitioning","hierarchical_partitioning20191204.R"))
source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","fig_3","plot_Figure_3_20191204.R"))

# load data
load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
                      "_aggregated_agg2",NA_now,"TRY_Env2_Kier_20191111.RData"))

TRY_Env_o <- TRY_Env2_Kier

# ---------------------------------------------------------------------------------------
# process data:
out <- prep_TRY_Env("Data_GapFilled",output_term,Rename_Vars,TRY_Env_o,spec_count)

info_2=out$info_2
trait_2=out$trait_2
soil_2=out$soil_2
climate_2=out$climate_2

# ---------------------------------------------------------------------------------------
# load analysis Ridge Regression
nruns=50
folds=10
output_term="_sel20_"
ncomp_PCA=20
do_PCA=TRUE

# ---------------------------------------------------------------------------------------
# define data set to be loaded
sel_now="soilAclimate"
nruns=50
folds=10
ncomp_PCA=20
do_PCA=TRUE
output_term="_sel20_"
sel_now="noiseAclimate"
sel_now="soilAnoise"
out_now <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,firstname,secondname,nruns,output_term,ncomp_PCA,do_PCA)  
# ---------------------------------------------------------------------------------------
# hierarchical partitioning
hp_now <- hierarchical_partitioning(origin,out_now,output_term,oldData,out_rf,model_now="Ridge_Regression",trait_2,sel_now)

# ---------------------------------------------------------------------------------------
res <- matrix(NA,ncol=6,nrow=17)
# prep output matrix
colnames(res)=c("Trait",	"Group"	,"Explained Variance by Soil and NOISE [r2]", 
                "Soil (Independent Effect) [r2]","NOISE (Independent Effect) [r2]","Total (soil and NOISE) Joint Effect [r2]")

res[,1] <-Rename_Vars(colnames(out_now$r2_soilAnoise))[,3]
res[,2] <- put_into_traitGroup(colnames(out_now$r2_soilAnoise))
res[,3] <- round(colMeans(out_now$r2_soilAnoise),digits=2)
res[,4] <- round(colMeans(hp_now$indep_soil-hp_now$joint_soil),digits=2)
res[,5] <- round(colMeans(hp_now$indep_noise-hp_now$joint_noise),digits=2)
res[,6] <- round(colMeans(hp_now$joint_soil*2),digits=2)
res <- as.data.frame(res)

res2=as.matrix(res)
res2 <- as.data.frame(res2)
res2 <- res2[order(res2$`Soil (Independent Effect) [r2]`,decreasing = T),]
require("xlsx")
write.xlsx(res2, file=file.path(origin,"data","Results","tables","Tab_S5","Table_S5a.xls"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)
#---------------------------------------------
sel_now="noiseAclimate"
out_now <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,firstname,secondname,nruns,output_term,ncomp_PCA,do_PCA)  
# ---------------------------------------------------------------------------------------
# hierarchical partitioning
hp_now <- hierarchical_partitioning(origin,out_now,output_term,oldData,out_rf,model_now="Ridge_Regression",trait_2,sel_now)


res <- matrix(NA,ncol=6,nrow=17)
colnames(res)=c("Trait",	"Group"	,"Explained Variance by NOISE and Climate [r2]", 
                "NOISE (Independent Effect) [r2]","Climate (Independent Effect) [r2]","Total (NOISE and Climate) Joint Effect [r2]")
res[,1] <-Rename_Vars(colnames(out_now$r2_noiseAclimate))[,3]
res[,2] <- put_into_traitGroup(colnames(out_now$r2_noiseAclimate))
res[,3] <- round(colMeans(out_now$r2_noiseAclimate),digits=2)
res[,4] <- round(colMeans(hp_now$indep_noise-hp_now$indep_noise),digits=2)
res[,5] <- round(colMeans(hp_now$indep_climate-hp_now$indep_climate),digits=2)
res[,6] <- round(colMeans(hp_now$indep_noise*2),digits=2)
res <- as.data.frame(res)

res2=as.matrix(res)
res2 <- as.data.frame(res2)
res2 <- res2[order(res2$`Climate (Independent Effect) [r2]`,decreasing = T),]
require("xlsx")
write.xlsx(res2, file=file.path(origin,"data","Results","tables","Tab_S5","Table_S5b.xls"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)
