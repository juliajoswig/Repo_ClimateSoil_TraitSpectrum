
plot_figure_S4_barplot <- function(origin){
  
  
  load(file.path(origin, "data", "_results","RidgeRegression","hp_RR",paste0("HP_",nruns=50,"nruns_","Data_GapFilled", "",".RData")))
  trait_now=1
  for(trait_now in 1:17){
    
    trait_name_now=colnames(hp_l[[1]][[1]])[trait_now]
    names(hp_l)
    names(hp_l)
    hp_up = hp_l[[which(names(hp_l)%in%"soilAclimate")]]
    hp_low1 = hp_l[[which(names(hp_l)%in%c("soil_physicsAsoil_chemistry","soil_chemistryAsoil_physics"))]]
    hp_low2 = hp_l[[which(names(hp_l)%in%c("climate_waterAclimate_energy"))]]
    #------------------------------------------
    indep_effects_subsoil = c(mean(hp_low1$indep_soil_physics[,trait_now]-hp_low1$joint_soil_physics[,trait_now]),
                              mean(hp_low1$indep_soil_chemistry[,trait_now]-hp_low1$joint_soil_physics[,trait_now]))
    indep_effects_subclimate = c(mean(hp_low2$indep_climate_water[,trait_now]-hp_low2$joint_climate_water[,trait_now]),
                                 mean(hp_low2$indep_climate_energy[,trait_now]-hp_low2$indep_climate_energy[,trait_now]))
    indep_effect_climate=mean(hp_up$indep_climate[,trait_now]-hp_up$joint_climate[,trait_now])
    indep_effect_soil=mean(hp_up$indep_soil[,trait_now]-hp_up$joint_soil[,trait_now])
    indep_effect_climateb <- indep_effects_subclimate*indep_effect_climate
    indep_effects_subsoilb <- indep_effects_subsoil*indep_effect_soil
    indep_effects_recalc=c(indep_effect_climateb,indep_effects_subsoilb)
    
    
    indep_soil_physics = colMeans(hp_low1[[1]]-hp_low1[[3]])
    indep_soil_physics[indep_soil_physics<0]=0
    indep_soil_chemistry = colMeans(hp_low1[[2]]-hp_low1[[3]])
    indep_soil_chemistry[indep_soil_chemistry<0]=0
    joint_soil_subs = colMeans(hp_low1[[3]]*2)
    #------------------------------------------
    indep_climate_water = colMeans(hp_low2[[1]]-hp_low2[[3]])
    indep_climate_water[indep_climate_water<0]=0
    indep_climate_energy = colMeans(hp_low2[[2]]-hp_low2[[3]])
    indep_climate_energy[indep_climate_energy<0]=0
    joint_climate_subs = colMeans(hp_low2[[3]]*2)
    #------------------------------------------
    indep_climate = colMeans(hp_up$indep_climate-hp_up$joint_climate)
    indep_climate[indep_climate<0]=0
    indep_soil = colMeans(hp_up$indep_soil-hp_up$joint_soil)
    indep_soil[indep_soil<0]=0
    joint = colMeans(hp_up$joint_soil*2)
    
    pdf(file = file.path(origin,"figures","figure_S4","all",paste0(colnames(hp_l[[1]][[1]])[trait_now],"_Barplot.pdf")),pointsize = 11,height=4,width=10)
    par(mfrow=c(1,3),mar=c(6,6,4,4))
    
    s=rbind(indep_soil_physics,joint_soil_subs,indep_soil_chemistry)
    c=rbind(indep_climate_water,joint_soil_subs,indep_climate_energy)
    tot=rbind(indep_soil,joint,indep_climate)
    
    colnames(s) <- Rename_Vars(colnames(s))[,3]
    
    barplot(as.matrix(tot[,trait_now]),
            las=3,ylim=c(0,1),ylab="Variance explained",
            col=c(soil_S4,"lightgray",climate_S4),cex.lab=2,cex.axis=2)
    axis(1,labels = "Climate and Soil",at = .7,tick = F,cex.axis=2)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c("climate", "joint", "soil"), col = c(climate_S4,"lightgray",soil_S4),cex=2,
                       text.col = "black",  pch = c(15, 15, 15),bg = "white")
           
    barplot(as.matrix(c[,trait_now]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",main= "",
            col=c(climtype1_S4,"lightgray",climtype2_S4))
    axis(1,labels = "Climate",at = .7,tick = F,cex.axis=2)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c("water", "joint", "energy"), col = c(climtype2_S4,"lightgray",climtype1_S4),cex=2,
           text.col = "black",  pch = c(15, 15, 15),bg = "white")
    
    barplot(as.matrix(s[,trait_now]),yaxt="n",
            las=3,ylim=c(0,1),ylab="",
            col=c(soiltype1_S4,"lightgray",soiltype2_S4))
    axis(1,labels = "Soil",at = .7,tick = F,cex.axis=2)
    abline(h = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),col="lightgray",lty=4)
    legend("topright", c("soil chemistry", "joint", "soil physics"), 
           col = c(soiltype2_S4,"lightgray",soiltype1_S4),cex=2,
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