
plot_Figure_S_PCA_HP <- function(origin,doPCA){
  output_term="PCA"
  
  list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results"))
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("R2_",nruns,"nruns_", output_term,pca_term,".RData")))

if(!dir.exists(file.path(origin,"figures","figure_S_HP_PCA"))){
  dir.create(file.path(origin,"figures","figure_S_HP_PCA"))}

pdf(file=file.path(origin,"figures","figure_S_HP_PCA","figure_S_HP_PCA.pdf"),width=10,height=4)
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
