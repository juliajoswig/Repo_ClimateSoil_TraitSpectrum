table_1 <- function(origin){

  library(xtable)
  
  nruns=50
  target_order=Rename_Vars(target_order1)[,3]
  output_term=""
  climOsoil="soilAclimate"
  doPCA=TRUE
# ---------------------------------------------------------------------------------------
# R2 data
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results"))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_", output_term,pca_term,".RData")))
  r2_now <- r2_l[[which(names(r2_l)%in%climOsoil)]]

# ---------------------------------------------------------------------------------------
# hierarchical partitioning data
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
  hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]

# ---------------------------------------------------------------------------------------
# prep output matrix
  res <- matrix(NA,ncol=6,nrow=17)
  colnames(res)=c("Trait",	"Group"	,"Explained Variance by Soil and Climate [r2]", 
                  "Soil (Independent Effect) [r2]","Climate (Independent Effect) [r2]","Total (Soil and Climate) Joint Effect [r2]")
  
  ix=match(table=colnames(r2_now$r2_soilAclimate),x = target_order1)
  colnames(r2_now$r2_soilAclimate)
  res[,1] <- Rename_Vars(colnames(r2_now$r2_soilAclimate))[,3]
  res[,2] <- put_into_traitGroup(colnames(r2_now$r2_soilAclimate))
  res[,3] <- round(colMeans(r2_now$r2_soilAclimate),digits=2)
  res[,4] <- round(colMeans(hp_now$indep_soil-hp_now$joint_soil),digits=2)
  res[,5] <- round(colMeans(hp_now$indep_climate-hp_now$joint_climate),digits=2)
  res[,6] <- round(colMeans(hp_now$joint_soil*2),digits=2)
  res <- as.data.frame(res)

  res2=as.matrix(res)
  for(i in 1:17){
    res2[i,3] <-  paste0(round(mean(r2_now$r2_soilAclimate[,i],na.rm = T),digits=2)," (",round(min(r2_now$r2_soilAclimate[,i],na.rm=T),digits=2),"-",round(max(r2_now$r2_soilAclimate[,i],na.rm=T),digits=2),")")
    res2[i,4] <-  paste0(round(mean(hp_now$indep_soil[,i]-hp_now$joint_soil[,i],na.rm = T),digits=2)," (",round(min(hp_now$indep_soil[,i]-hp_now$joint_soil[,i],na.rm=T),digits=2),"-",round(max(hp_now$indep_soil[,i]-hp_now$joint_soil[,i],na.rm=T),digits=2),")")
    res2[i,5] <-  paste0(round(mean(hp_now$indep_climate[,i]-hp_now$joint_climate[,i],na.rm = T),digits=2)," (",round(min(hp_now$indep_climate[,i]-hp_now$joint_climate[,i],na.rm=T),digits=2),"-",round(max(hp_now$indep_climate[,i]-hp_now$joint_climate[,i],na.rm=T),digits=2),")")
    res2[i,6] <-  paste0(round(mean(hp_now$joint_soil[,i]*2,na.rm = T),digits=2)," (",round(min(hp_now$joint_soil[,i]*2,na.rm=T),digits=2),"-",round(max(hp_now$joint_soil[,i]*2,na.rm=T),digits=2),")")
  }
  res2 <- as.data.frame(res2)

  res2 <- res2[ix,]

  if(!file.exists(file.path(origin, "tables"))){
    dir.create(file.path(origin, "tables"))}
  if(!file.exists(file.path(origin, "tables","Table_1"))){
    dir.create(file.path(origin, "tables","Table_1"))}
  
  write.csv(res2, file=file.path(origin,"tables","Table_1","Table_1.csv"))
  print(xtable(res2, type = "latex"), file =file.path(origin,"tables","Table_1","Table_1.tex"))
  #print(xtable(res2, type = "latex"))
  return(res2)
}