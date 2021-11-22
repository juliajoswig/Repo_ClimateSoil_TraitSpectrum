
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

if(!file.exists(file.path(origin,"figures","Supplement_Fig_18"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_18"))}
if(!file.exists(file.path(origin,"figures","Supplement_Fig_18","Barplot"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_18","Barplot"))}

  list.files(file.path(origin,"data","_results","RidgeRegression","_RidgeRegression_results"))
  nruns=50
  output_term=""
  doPCA=TRUE
  if(doPCA){doPCA_term="doPCA"}
  if(!doPCA){doPCA_term=""}
  load(file.path(origin,"data","_results","RidgeRegression","_RidgeRegression_results",paste0("HP_",nruns,"nruns_",output_term,doPCA_term,".RData")))
  load(file.path(origin,"data","_results","RidgeRegression","_RidgeRegression_results",paste0("R2_",nruns,"nruns_",output_term,doPCA_term,".RData")))  
  hp_up = hp_l[[which(names(hp_l)%in%"soilAclimate")]]
  hp_low1 = hp_l[[which(names(hp_l)%in%c("climate_waterAclimate_energy"))]]
  hp_low2 = hp_l[[which(names(hp_l)%in%c("soil_chemistryAsoil_physics"))]]
  
  r2_up = r2_l[[which(names(r2_l)%in%"soilAclimate")]]
  r2_low1 = r2_l[[which(names(r2_l)%in%c("climate_waterAclimate_energy"))]]
  r2_low2 = r2_l[[which(names(r2_l)%in%c("soil_chemistryAsoil_physics"))]]
  
  #------------------------------------------
  # pure independent effect of subgroups climate (indep - joint) & joint
  all_effects_climate = rbind(colMeans(hp_low1$indep_climate_water - hp_low1$joint_climate_water),
                              colMeans(hp_low1$joint_climate_water + hp_low1$joint_climate_energy),
                              colMeans(hp_low1$indep_climate_energy - hp_low1$joint_climate_energy))
  rownames(all_effects_climate) <- c("climate water","joint","climate energy")
  # pure independent effect of subgroups soil (indep - joint) & joint
  all_effects_soil = rbind(colMeans(hp_low2$indep_soil_chemistry - hp_low2$joint_soil_chemistry),
                           colMeans(hp_low2$joint_soil_chemistry + hp_low2$joint_soil_physics),
                           colMeans(hp_low2$indep_soil_physics - hp_low2$joint_soil_physics))
  rownames(all_effects_soil) <- c("soil chemistry","joint","soil physics")
  #------------------------------------------
  # pure independent effect of subgroups total (indep - joint) & joint
  all_effects_tot = rbind(colMeans(hp_up$indep_climate - hp_up$joint_climate),
                          colMeans(hp_up$joint_climate + hp_up$joint_soil),
                          colMeans(hp_up$indep_soil - hp_up$joint_soil))
  rownames(all_effects_tot) <- c("climate","joint","soil")
  # check:
#  plot(colSums(all_effects_climate),colSums(all_effects_soil) )
#  plot(all_effects_tot[3,],colSums(all_effects_soil) )
#  plot(all_effects_tot[1,],colSums(all_effects_climate) )
#  plot(all_effects_tot[1,],all_effects_tot[3,]) 
  

  
  trait_now=6
  for(trait_now in 1:17){
    
    trait_name_now = colnames(all_effects_soil)[trait_now]
    
    pdf(file = file.path(origin,"figures","Supplement_Fig_18","Barplot",paste0(colnames(all_effects_soil)[trait_now],"_Barplot.pdf")),
        pointsize = 11,height=4,width=10)
    par(mfrow=c(1,3),mar=c(3.5,6,1.5,1))
    
    s=all_effects_soil
    s[s<0]=0
    c=all_effects_climate
    c[c<0]=0
    tot=all_effects_tot
    tot[tot<0]=0
    
    colnames(s) <- Rename_Vars(colnames(s))[,3]
    
    barplot(as.matrix(tot[3:1,trait_now]),
            las=3,ylim=c(0,1),ylab="Variance explained",
            col=c(soil_S4,"lightgray",climate_S4),cex.lab=3,cex.axis=2)
    axis(1,labels = "Climate and Soil",at = .7,tick = F,cex.axis=3,line = 1)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(tot)), col = c(climate_S4,"lightgray",soil_S4),cex=2,box.col = "lightgray",
                       text.col = "black",  pch = c(15, 15, 15),bg = "white")
           
    barplot(as.matrix(c[3:1,trait_now]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",main= "",
            col=c(climtype2_S4,"lightgray",climtype1_S4))
    axis(1,labels = "Climate",at = .7,tick = F,cex.axis=3,line = 1)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(c)), col =c(climtype1_S4,"lightgray",climtype2_S4),cex=2,box.col = "lightgray",
           text.col = "black",  pch = c(15, 15, 15),bg = "white")
    
    barplot(as.matrix(s[3:1,trait_now]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",
            col=c(soiltype2_S4,"lightgray",soiltype1_S4))
    axis(1,labels = "Soil",at = .7,tick = F,cex.axis=3,line = 1)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(s)), col =c(soiltype1_S4,"lightgray",soiltype2_S4),cex=2,box.col = "lightgray",
           text.col = "black",  pch = c(15, 15, 15),bg = "white")
    
     dev.off()
}