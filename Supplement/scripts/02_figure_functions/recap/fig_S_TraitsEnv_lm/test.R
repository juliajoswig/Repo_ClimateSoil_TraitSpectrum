

#------------------------------------
doPCA=FALSE
nruns=2
type_analysis="lm"
output_term="coord"
climOsoil="soilAclimate"

if(doPCA){pca_term="doPCA"}
if(!doPCA){pca_term="no_PCA"}
list.files(file.path(origin, "data", "_results","lm",climOsoil, paste0(nruns,"Reps")))
load(file=file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                    paste0("Res_",output_term,pca_term,".RData")))
coordr2 <- out$r2_vars

#------------------------------------
doPCA=FALSE
nruns=2
type_analysis="lm"
output_term="grid"
climOsoil="soilAclimate"

if(doPCA){pca_term="doPCA"}
if(!doPCA){pca_term="no_PCA"}
list.files(file.path(origin, "data", "_results","lm",climOsoil, paste0(nruns,"Reps")))
load(file=file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                    paste0("Res_",output_term,pca_term,".RData")))

gridr2 <- out$r2_vars

#------------------------------------
doPCA=FALSE
nruns=10
type_analysis="lm"
output_term=""
climOsoil="soilAclimate"

if(doPCA){pca_term="doPCA"}
if(!doPCA){pca_term="no_PCA"}
list.files(file.path(origin, "data", "_results","lm",climOsoil, paste0(nruns,"Reps")))
load(file=file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                    paste0("Res_",output_term,pca_term,".RData")))
ERr2 <- out$r2_vars

#------------------------------------
doPCA=FALSE
nruns=2
type_analysis="lm"
output_term="biome"
climOsoil="soilAclimate"

if(doPCA){pca_term="doPCA"}
if(!doPCA){pca_term="no_PCA"}
list.files(file.path(origin, "data", "_results","lm",climOsoil, paste0(nruns,"Reps")))
load(file=file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                    paste0("Res_",output_term,pca_term,".RData")))
Biomer2 <- out$r2_vars
dim(Biomer2)

tr=6
pdf(file=file.path(origin,"figures","figure_S_TraitsEnv_lm",paste0("Test2",output_term,".pdf")))
    par(mfrow=c(1,4))
    
    length(Biomer2[tr,])
    length(ERr2[tr,])
    length(gridr2[tr,])
    length(coordr2[tr,])
    
for(tr in 1:17){
  dat_combo <-  cbind(ERr2[tr,],gridr2[tr,],coordr2[tr,])
  colnames(dat_combo) <- c("ER","grid","coord")
  boxplot(Biomer2[tr,],ylab="Biome",main=rownames(ERr2)[tr],ylim=c(0,1))
  abline(h=median(Biomer2[tr,]),col="red")
  boxplot(ERr2[tr,],ylab="ER",main=rownames(ERr2)[tr],ylim=c(0,1))
  abline(h=median(ERr2[tr,]),col="red")
  boxplot(gridr2[tr,],ylab="Grid",main=rownames(ERr2)[tr],ylim=c(0,1))
  abline(h=median(gridr2[tr,]),col="red")
  boxplot(coordr2[tr,],ylab="Coord",main=rownames(ERr2)[tr],ylim=c(0,1))
  abline(h=median(coordr2[tr,]),col="red")
}
dev.off()


pdf(file=file.path(origin,"figures","figure_S_TraitsEnv_lm",paste0("Test3",output_term,".pdf")),width=12,height=5)
par(mfrow=c(1,8))

length(Biomer2[tr,])
length(ERr2[tr,])
length(gridr2[tr,])
length(coordr2[tr,])

for(tr in 1:17){
  dat_combo <-  cbind(ERr2[tr,],gridr2[tr,],coordr2[tr,])
  colnames(dat_combo) <- c("ER","grid","coord")
  boxplot(Biomer2[tr,Rename_Vars(colnames(Biomer2))[,2]=="soil"],ylab="Biome soil",main=rownames(ERr2)[tr],ylim=c(0,1),col=soil_col)
  abline(h=median(Biomer2[tr,Rename_Vars(colnames(Biomer2))[,2]=="soil"]),col="red")
  boxplot(Biomer2[tr,Rename_Vars(colnames(Biomer2))[,2]=="climate"],ylab="Biome climate",main=rownames(ERr2)[tr],ylim=c(0,1),
          col=climate_col)
  abline(h=median(Biomer2[tr,Rename_Vars(colnames(Biomer2))[,2]=="climate"]),col="red")

  boxplot(ERr2[tr,Rename_Vars(colnames(ERr2))[,2]=="soil"],ylab="ER soil",main=rownames(ERr2)[tr],ylim=c(0,1),col=soil_col)
  abline(h=median(ERr2[tr,Rename_Vars(colnames(ERr2))[,2]=="soil"]),col="red")
  boxplot(ERr2[tr,Rename_Vars(colnames(ERr2))[,2]=="climate"],ylab="ER climate",main=rownames(ERr2)[tr],ylim=c(0,1),col=climate_col)
  abline(h=median(ERr2[tr,Rename_Vars(colnames(ERr2))[,2]=="climate"]),col="red")
  
  boxplot(gridr2[tr,Rename_Vars(colnames(gridr2))[,2]=="soil"],ylab="grid soil",main=rownames(ERr2)[tr],ylim=c(0,1),col=soil_col)
  abline(h=median(gridr2[tr,Rename_Vars(colnames(ERr2))[,2]=="soil"]),col="red")
  boxplot(gridr2[tr,Rename_Vars(colnames(gridr2))[,2]=="climate"],ylab="grid climate",main=rownames(ERr2)[tr],ylim=c(0,1),col=climate_col)
  abline(h=median(gridr2[tr,Rename_Vars(colnames(ERr2))[,2]=="climate"]),col="red")
  
  boxplot(coordr2[tr,Rename_Vars(colnames(coordr2))[,2]=="soil"],ylab="coord soil",main=rownames(ERr2)[tr],ylim=c(0,1),col=soil_col)
  abline(h=median(coordr2[tr,Rename_Vars(colnames(coordr2))[,2]=="soil"]),col="red")
  boxplot(coordr2[tr,Rename_Vars(colnames(coordr2))[,2]=="climate"],ylab="coord climate",main=rownames(ERr2)[tr],ylim=c(0,1),col=climate_col)
  abline(h=median(coordr2[tr,Rename_Vars(colnames(coordr2))[,2]=="climate"]),col="red")
  
}
dev.off()
