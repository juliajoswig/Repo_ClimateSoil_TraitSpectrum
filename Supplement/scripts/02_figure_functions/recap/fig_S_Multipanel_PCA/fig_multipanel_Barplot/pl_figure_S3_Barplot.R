
plot_figure_PCA_multipael_barplot <- function(origin){
  
  output_term="PCA"
  list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results"))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("HP_",50,"nruns_", output_term,"doPCA",".RData")))
  load(file.path(origin,  "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("R2_",50,"nruns_", output_term,"doPCA",".RData")))
  
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
  
  
  if(!dir.exists(file.path(origin,"figures","figure_multipanel_PCA"))){
    dir.create(file.path(origin,"figures","figure_multipanel_PCA"))}
  
  if(!dir.exists(file.path(origin,"figures","figure_multipanel_PCA","all"))){
    dir.create(file.path(origin,"figures","figure_multipanel_PCA","all"))}
  
  trait_now=3
  for(trait_now in 1:ncol(r2_up[[1]])){
    
    trait_name_now = colnames(all_effects_soil)[trait_now]
    
    pdf(file = file.path(origin,"figures","figure_multipanel_PCA","all",paste0(colnames(all_effects_soil)[trait_now],"_Barplot.pdf")),pointsize = 11,height=4,width=10)
    par(mfrow=c(1,3),mar=c(2.5,5,1.5,1))
    
    s=all_effects_soil
    s[s<0]=0
    c=all_effects_climate
    c[c<0]=0
    tot=all_effects_tot
    tot[tot<0]=0
    
    colnames(s) <- Rename_Vars(colnames(s))[,3]
    
    barplot(as.matrix(tot[3:1,trait_now]),
            las=3,ylim=c(0,1),ylab="Variance explained",
            col=c(soil_S4,"lightgray",climate_S4),cex.lab=2,cex.axis=2)
    axis(1,labels = "Climate and Soil",at = .7,tick = F,cex.axis=2)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(tot)), col = c(climate_S4,"lightgray",soil_S4),cex=2,box.col = "lightgray",
                       text.col = "black",  pch = c(15, 15, 15),bg = "white")
           
    barplot(as.matrix(c[3:1,trait_now]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",main= "",
            col=c(climtype2_S4,"lightgray",climtype1_S4))
    axis(1,labels = "Climate",at = .7,tick = F,cex.axis=2)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(c)), col =c(climtype1_S4,"lightgray",climtype2_S4),cex=2,box.col = "lightgray",
           text.col = "black",  pch = c(15, 15, 15),bg = "white")
    
    barplot(as.matrix(s[3:1,trait_now]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",
            col=c(soiltype2_S4,"lightgray",soiltype1_S4))
    axis(1,labels = "Soil",at = .7,tick = F,cex.axis=2)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c(rownames(s)), col =c(soiltype1_S4,"lightgray",soiltype2_S4),cex=2,box.col = "lightgray",
           text.col = "black",  pch = c(15, 15, 15),bg = "white")
    
     dev.off()
}

}
  
  # s=rbind(indep_soil_physics,joint_soil_subs,indep_soil_chemistry)
  # colnames(s) <- Rename_Vars(colnames(s))[,3]
  # 
  # barplot(s,
  #         las=3,ylim=c(0,1),ylab="Variance explained",space = c(.5),
  #         col=c(soiltype1_S4,"lightgray",soiltype2_S4))
  # abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
  # abline(v = seq(from = .25,to = 30,by = 1.5),col="lightgray",lty=4)
  # legend("topright", c("soil chemistry", "joint", "soil physics"), col = c(soiltype2_S4,"lightgray",soiltype1_S4),
  #        text.col = "black",  pch = c(15, 15, 15),bg = "white")
  # 
  # 
  # c=rbind(indep_climate_water,joint_soil_subs,indep_climate_energy)
  # colnames(c) <- Rename_Vars(colnames(c))[,3]
  # barplot(c,
  #         las=3,ylim=c(0,1),ylab="Variance explained",space = c(.5),
  #         col=c(climtype1_S4,"lightgray",climtype2_S4))
  # abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
  # abline(v = seq(from = .25,to = 30,by = 1.5),col="lightgray",lty=4)
  # legend("topright", c("water", "joint", "energy"), col = c(climtype2_S4,"lightgray",climtype1_S4),
  #        text.col = "black",  pch = c(15, 15, 15),bg = "white")
  # 
  # #     barplot(rbind(indep_soil,joint,indep_climate),
  # #            las=3,ylim=c(0,1),ylab="Variance explained",
  # #            col=c(soil_S4,"lightgray",climate_S4))
  # #     legend("topright", c("climate", "joint", "soil"), col = c(climate_S4,"lightgray",soil_S4),
  # #            text.col = "black",  pch = c(15, 15, 15),bg = "white")
  # 
  #    s=rbind(indep_soil_physics,joint_soil_subs,indep_soil_chemistry)
  #    tot=rbind(indep_soil,joint,indep_climate)
  #    plot_data=cbind(tot[,1],s[,1],c[,1],
  #                    tot[,2],s[,2],c[,2],
  #                    tot[,3],s[,3],c[,3],
  #                    tot[,4],s[,4],c[,4],
  #                    tot[,5],s[,5],c[,5],
  #                    tot[,6],s[,6],c[,6],
  #                    tot[,7],s[,7],c[,7],
  #                    tot[,8],s[,8],c[,8],
  #                    tot[,9],s[,9],c[,9],
  #                    tot[,10],s[,10],c[,10],
  #                    tot[,11],s[,11],c[,11],
  #                    tot[,12],s[,12],c[,12],
  #                    tot[,13],s[,13],c[,13],
  #                    tot[,14],s[,14],c[,14],
  #                    tot[,15],s[,15],c[,15],
  #                    tot[,16],s[,16],c[,16],
  #                    tot[,17],s[,17],c[,17]
  #                    )
  #    dev.off()
  #    barplot(plot_data,
  #            col=c(climate_S4,"lightgray",soil_S4,
  #                  soiltype1_S4,"lightgray",soiltype2_S4,
  #                  climtype2_S4,"lightgray",climtype1_S4))
  # 
  #  