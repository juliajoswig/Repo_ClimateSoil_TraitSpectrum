


# ---------------------------------------------------------------------------------------
setwd("/Net/Groups")
orig_loctem =  "BGI"#"/Volumes/BGI" 
output_term = "" # or "non_woody" or  "woody" or ""
version_now="V_2020_02_10"
require("dplyr")
Appr_type_now="Data_GapFilled"

# ---------------------------------------------------------------------------------------
# define the origin path
# ---------------------------------------------------------------------------------------
# origin = # please add your local path here & comment the ones below.
#origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
origin = file.path("/Users/jjoswig/Documents/_docs/03_projects/2020/002_Dichotomy/2019_Revision/20191004_Revision/R")

source(file.path(origin,"scripts",version_now ,"_master","fn_functions.R"))


# load data
list.files(file.path(origin,"data/master_matrix","_aggregated_agg2"))
load(file = file.path(origin,"data/master_matrix","_aggregated_agg2","NA_mnNbr","TRY_Env2_Kier_20191111.RData"))

TRY_Env_o <- TRY_Env2_Kier

# ---------------------------------------------------------------------------------------
# process data:
out <- prep_TRY_Env("Data_GapFilled",output_term,Rename_Vars,TRY_Env_o)

info_2=out$info_2
trait_2=out$trait_2
soil_2=out$soil_2
climate_2=out$climate_2

# ---------------------------------------------------------------------------------------
# load analyses

nruns=50
folds=10
output_term=""
ncomp_PCA=20
do_PCA=TRUE
climOsoil="soil_chemistryAsoil_physics"
#climOsoil="climate_waterAclimate_energy"
ncomp_PCA=20


# ---------------------------------------------------------------------------------------
# R2 data
list.files(file.path(origin, "data", "_results","RidgeRegression","hp_RR"))
load(file.path(origin, "data", "_results","RidgeRegression","hp_RR",paste0("R2_",nruns=nruns,"nruns_","Data_GapFilled", output_term,".RData")))
r2_now <- r2_l[[which(names(r2_l)%in%climOsoil)]]

# ---------------------------------------------------------------------------------------
# hierarchical partitioning data
load(file.path(origin, "data", "_results","RidgeRegression","hp_RR",paste0("HP_",nruns=nruns,"nruns_","Data_GapFilled", output_term,".RData")))
hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]


# ---------------------------------------------------------------------------------------
# prep output matrix
res <- matrix(NA,ncol=6,nrow=17)
if(climOsoil=="soil_chemistryAsoil_physics"){
colnames(res)=c("Trait",	"Group"	,"Explained Variance by soil chemistry and soil physics [r2]", 
                "soil chemistry (Independent Effect) [r2]","soil physics (Independent Effect) [r2]","Total (soil chemistry and soil physics) Joint Effect [r2]")
}
if(climOsoil=="climate_waterAclimate_energy"){
  colnames(res)=c("Trait",	"Group"	,"Explained Variance by climate water and climate energy [r2]", 
                  "Climate water (Independent Effect) [r2]","Climate energy (Independent Effect) [r2]","Total (climate water and climate energy) Joint Effect [r2]")
}

# set to 0 if smaller than 0
hp_now[[1]][hp_now[[1]]<0]=0
hp_now[[2]][hp_now[[2]]<0]=0
hp_now[[3]][hp_now[[3]]<0]=0

res[,1] <-Rename_Vars(colnames(r2_now[[3]]))[,3]
res[,2] <- put_into_traitGroup(colnames(r2_now[[3]]))
res[,3] <- round(colMeans(r2_now[[3]]),digits=2)
res[,4] <- round(colMeans(hp_now[[1]]-hp_now[[3]]),digits=2)
res[,5] <- round(colMeans(hp_now[[2]]-hp_now[[4]]),digits=2)
res[,6] <- round(colMeans(hp_now[[1]]*2),digits=2)
res <- as.data.frame(res)

res2=as.matrix(res)
i
for(i in 1:17){
res2[i,3] <-  paste0(round(mean(r2_now[[3]][,i],na.rm = T),digits=2)," (",round(min(r2_now[[3]][,i],na.rm=T),digits=2),"-",round(max(r2_now[[3]][,i],na.rm=T),digits=2),")")
res2[i,4] <-  paste0(round(mean(hp_now[[1]][,i]-hp_now[[3]][,i],na.rm = T),digits=2)," (",round(min(hp_now[[1]][,i]-hp_now[[3]][,i],na.rm=T),digits=2),"-",round(max(hp_now[[1]][,i]-hp_now[[3]][,i],na.rm=T),digits=2),")")
res2[i,5] <-  paste0(round(mean(hp_now[[2]][,i]-hp_now[[4]][,i],na.rm = T),digits=2)," (",round(min(hp_now[[2]][,i]-hp_now[[4]][,i],na.rm=T),digits=2),"-",round(max(hp_now[[2]][,i]-hp_now[[4]][,i],na.rm=T),digits=2),")")
res2[i,6] <-  paste0(round(mean(hp_now[[3]][,i]*2,na.rm = T),digits=2)," (",round(min(hp_now[[3]][,i]*2,na.rm=T),digits=2),"-",round(max(hp_now[[3]][,i]*2,na.rm=T),digits=2),")")
}
res2 <- as.data.frame(res2)
res2 <- res2[order(put_into_traitGroup(colnames(r2_now[[1]]))),]

write.csv(res2, file=file.path(origin,"tables","Table_Sx_Subgroups",paste0("Table_Sx",climOsoil,".csv")))

soilChem <- mean(hp_now[[1]]-hp_now[[3]])*100
soilPhys <- mean(hp_now[[2]]-hp_now[[3]])*100

Eco_1sub <- colMeans(hp_now[[1]]-hp_now[[3]])[put_into_traitGroup(colnames(r2_now[[3]]))=="Eco"]
Eco_1sub[Eco_1sub<0]=0
mean(Eco_1sub)
Size_1sub <- colMeans(hp_now[[1]]-hp_now[[3]])[put_into_traitGroup(colnames(r2_now[[3]]))=="Size"]
Size_1sub[Size_1sub<0]=0
mean(Size_1sub)
