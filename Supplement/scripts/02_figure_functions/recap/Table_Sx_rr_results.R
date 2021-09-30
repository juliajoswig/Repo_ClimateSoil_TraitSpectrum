
setwd("/Net/Groups")
orig_loctem =  "BGI"#"/Volumes/BGI" 
#orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
output_term="_sel20_"#_minmaxmed
spec_count=20 # if output_term == "_sel20_"
NA_now="NA_mnNbr"#"NA_stays_NA"
# define the origin path
origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")

# ---------------------------------------------------------------------------------------
# load analysis Ridge Regression
# ---------------------------------------------------------------------------------------
nruns=50
folds=10
output_term="_sel20_"
ncomp_PCA=20
do_PCA=TRUE
source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegression20191111.R"))
out_RR <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,nruns,output_term,ncomp_PCA,do_PCA)  

source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","hierarchical_partitioning","hierarchical_partitioning20191120.R"))
sel_now="soilAclimate"
hp_rr_soilAclimate <- hierarchical_partitioning(origin,out_Carspls,out_RR,out_PLS,out_CARS_PLS,output_term,oldData,out_rf,model_now="Ridge_Regression",trait_2,sel_now)
hp_now = hp_rr_soilAclimate

res <- matrix(NA,ncol=13,nrow=17)
colnames(res)=c("name","r2_tot","r2_soil","r2_climate","pure.indep_soil","pure.indep_climate",
                "indep_soil","indep_climate","joint","distr_indep_soil","distr_indep_climate",
                "distr_pure.indep_soil","distr_pure.indep_climate")

res[,1] <- colnames(out_RR$r2_soilAclimate)
res[,2] <- round(colMeans(out_RR$r2_soilAclimate),digits=2)
res[,3] <- round(colMeans(out_RR$r2_soil),digits=2)
res[,4] <- round(colMeans(out_RR$r2_climate),digits=2)
res[,5] <- round(colMeans(hp_rr_soilAclimate$indep_1-hp_rr_soilAclimate$joint_1),digits=2)
res[,6] <- round(colMeans(hp_rr_soilAclimate$indep_2-hp_rr_soilAclimate$joint_2),digits=2)
res[,7] <- round(colMeans(hp_rr_soilAclimate$indep_1),digits=2)
res[,8] <- round(colMeans(hp_rr_soilAclimate$indep_2),digits=2)
res[,9] <- round(colMeans(hp_rr_soilAclimate$joint_1),digits=2)
res[,10] <- round(colMeans(hp_rr_soilAclimate$distr_indep2),digits=2)
res[,11] <- round(colMeans(hp_rr_soilAclimate$distr_indep1),digits=2)
res[,12] <- round(100*colMeans(((hp_rr_soilAclimate$indep_1-hp_rr_soilAclimate$joint_1)/
                            ((hp_rr_soilAclimate$indep_1-hp_rr_soilAclimate$joint_1)+hp_rr_soilAclimate$indep_2-hp_rr_soilAclimate$joint_2))),digits=2)
res[,13] <- round(100*colMeans(((hp_rr_soilAclimate$indep_2-hp_rr_soilAclimate$joint_2)/
                              ((hp_rr_soilAclimate$indep_1-hp_rr_soilAclimate$joint_1)+hp_rr_soilAclimate$indep_2-hp_rr_soilAclimate$joint_2))),digits=2)
res <- as.data.frame(res)
plot(res[,12],res[,13],col=color_to_traits(res[,1]),pch=16)
plot(res[,5],res[,6],col=color_to_traits(res[,1]),pch=16)
res2=as.matrix(res)
i
for(i in 1:17){
res2[i,1] <- colnames(out_RR$r2_soilAclimate)[i]
res2[i,2] <- paste0(round(mean(out_RR$r2_soilAclimate[,i],na.rm = T),digits=2)," (",round(min(out_RR$r2_soilAclimate[,i],na.rm=T),digits=2),"-",round(max(out_RR$r2_soilAclimate[,i],na.rm=T),digits=2),")")
res2[i,3] <-  paste0(round(mean(out_RR$r2_soil[,i],na.rm = T),digits=2)," (",round(min(out_RR$r2_soil[,i],na.rm=T),digits=2),"-",round(max(out_RR$r2_soil[,i],na.rm=T),digits=2),")")
res2[i,4] <-  paste0(round(mean(out_RR$r2_climate[,i],na.rm = T),digits=2)," (",round(min(out_RR$r2_climate[,i],na.rm=T),digits=2),"-",round(max(out_RR$r2_climate[,i],na.rm=T),digits=2),")")
res2[i,5] <-  paste0(round(mean(hp_rr_soilAclimate$indep_2[,i],na.rm = T),digits=2)," (",round(min(hp_rr_soilAclimate$indep_2,na.rm=T),digits=2),"-",round(max(hp_rr_soilAclimate$indep_2,na.rm=T),digits=2),")")
res2[i,6] <-  paste0(round(mean(hp_rr_soilAclimate$indep_1[,i],na.rm = T),digits=2)," (",round(min(hp_rr_soilAclimate$indep_1[,i],na.rm=T),digits=2),"-",round(max(hp_rr_soilAclimate$indep_1[,i],na.rm=T),digits=2),")")
res2[i,7] <-  paste0(round(mean(hp_rr_soilAclimate$joint_1[,i],na.rm = T),digits=2)," (",round(min(hp_rr_soilAclimate$joint_1[,i],na.rm=T),digits=2),"-",round(max(hp_rr_soilAclimate$joint_1[,i],na.rm=T),digits=2),")")
res2[i,8] <-  paste0(round(mean(hp_rr_soilAclimate$distr_joint_2[,i],na.rm = T),digits=2)," (",round(min(hp_rr_soilAclimate$distr_joint_2[,i],na.rm=T),digits=2),"-",round(max(hp_rr_soilAclimate$distr_joint_2[,i],na.rm=T),digits=2),")")
res2[i,9] <-  paste0(round(mean(hp_rr_soilAclimate$distr_joint_1[,i],na.rm = T),digits=2)," (",round(min(hp_rr_soilAclimate$distr_joint_1[,i],na.rm=T),digits=2),"-",round(max(hp_rr_soilAclimate$distr_joint_1[,i],na.rm=T),digits=2),")")
}
res2 <- as.data.frame(res2)

require("xlsx")
write.xlsx(res, file=file.path(origin,"data","Results","tables","Table_Sx_RR_results.xls"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)
