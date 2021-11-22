
# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
# origin = # please add your local path here & comment the ones below.
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Supplement" # please add your local path here 
list.files(file.path(origin,"scripts/_master"))

# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
# packages
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))

output_term="PCA"
  
  list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results"))
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("R2_",nruns,"nruns_", output_term,pca_term,".RData")))

if(!dir.exists(file.path(origin,"figures","Supplement_Fig_14"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_14"))}
{
pdf(file=file.path(origin,"figures","Supplement_Fig_14","figure_S_HP_PCA.pdf"),width=10,height=4)
  par(mfrow=c(1,5))
  barplot(rbind(colMeans(hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate)[1],
                colMeans(hp_l$soilAclimate$joint_climate*2)[1],
                colMeans(hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil)[1]),beside = F,
          col=c(climate_col,"lightgray",soil_col),ylim=c(0,1),ylab="Variance explained (r2)")
  
  barplot(rbind(colMeans(hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate)[2],
                colMeans(hp_l$soilAclimate$joint_climate*2)[2],
                colMeans(hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil)[2]),beside = F, 
          col=c(climate_col,"lightgray",soil_col),ylim=c(0,1),ylab="Variance explained (r2)")
  
  barplot(rbind(colMeans(hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate)[3],
                colMeans(hp_l$soilAclimate$joint_climate*2)[3],
                colMeans(hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil)[3]),beside = F, 
          col=c(climate_col,"lightgray",soil_col),ylim=c(0,1),ylab="Variance explained (r2)")
  
  barplot(rbind(colMeans(hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate)[4],
                colMeans(hp_l$soilAclimate$joint_climate*2)[4],
                colMeans(hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil)[4]),beside = F, 
          col=c(climate_col,"lightgray",soil_col),ylim=c(0,1),ylab="Variance explained (r2)")
  
  barplot(rbind(colMeans(hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate)[5],
                colMeans(hp_l$soilAclimate$joint_climate*2)[5],
                colMeans(hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil)[5]),beside = F, 
          col=c(climate_col,"lightgray",soil_col),ylim=c(0,1),ylab="Variance explained (r2)")
dev.off()

}
#rbind(colMeans(hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate)[1],
#      colMeans(hp_l$soilAclimate$joint_climate*2)[1],
#      colMeans(hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil)[1])
#rbind(colMeans(hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate)[2],
#      colMeans(hp_l$soilAclimate$joint_climate*2)[2],
#      colMeans(hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil)[2])
