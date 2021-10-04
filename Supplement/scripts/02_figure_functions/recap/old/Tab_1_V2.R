Tab_1 <- function(climOsoil,Appr_type, meanOsd){
  
  meanOsd="median"
  climOsoil="climateAsoil"
  Appr_type="General_"
  sel_now=""
  
  #------------------------------------------------------------------------------------------------
  # load VarSelOutput & get r2 rel importances
  #------------------------------------------------------------------------------------------------
  load(file.path(origin, "data", "VarSelect_output", paste0(nruns,"Reps"), fold_now,
                  paste0("VarSelOutput_",Appr_type,climOsoil,".RData")))
  
  r2_2 <- VarSelOutput$dat_plot$r2# rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="r2_mean")] 
  soil_2 <- VarSelOutput$dat_plot$soil#VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="soil_mean")] 
  atm_2 <- VarSelOutput$dat_plot$atm #VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="atm_mean")] 
  nb_2 <- VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="nbPred_mean")] 
    
  r2_2max <- VarSelOutput$dat_plot$r2_95qnt #VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="r2_95qnt")] 
  soil_2max <- VarSelOutput$dat_plot$soilx# VarSelOutput$dat_plot$soilx#VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="soil_95qnt")] 
  atm_2max <- VarSelOutput$dat_plot$atmx# VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="atm_95qnt")] 
  nb_2max <- VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="nbPred_95qnt")] 
  
  r2_2min <- VarSelOutput$dat_plot$r2_5qnt # VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="r2_5qnt")]  
  soil_2min <- VarSelOutput$dat_plot$soili#VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="soil_5qnt")] 
  atm_2min <- VarSelOutput$dat_plot$atmi#VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="atm_5qnt")] 
  nb_2min <- VarSelOutput$rel_I_metrics[,which(colnames(VarSelOutput$rel_I_metrics)=="nbPred_5qnt")] 
  
 
  #------------------------------------------------------------------------------------------------
  # weight the relative importance with r2
  #------------------------------------------------------------------------------------------------
  plot(soil_2+atm_2,pch=16)
  points(r2_2,col="red")
  
#  soil_2 <- soil_2*r2_2
#  atm_2 <-  atm_2*r2_2
  
#  soil_2min <- soil_2min*r2_2
#  atm_2min <-  atm_2min*r2_2
  
#  soil_2max <- r2_2max*r2_2
#  atm_2max <-  atm_2max*r2_2
  
  results_table <- matrix(NA,ncol=6,nrow=length(trait.names));colnames(results_table)=c("trait","r2","group","rel_I climate","rel_I soil","nb_pred")
  #rownames(results_table) <- trait.names
  
  trait.names[5] <- "LeN_"
  results_table[,1] <- gsub(x = VarSelOutput$colnames_resp,pattern =  paste0("_",meanOsd),replacement = "")#gsub(x = rownames(as.matrix(r2_2)),pattern =  paste0("_",meanOsd),replacement = "")
  results_table[,2] <- round(r2_2,digits = 3)
  results_table[,3] <- put_into_traitGroup(results_table[,1])
  results_table[,4] <- round(atm_2,digits = 3)
  results_table[,5] <- round(soil_2,digits = 3)
  
  results_table_adv <- results_table
  t=1
  results_table_adv[,1] <- rename_for_plot(results_table_adv[,1])
  results_table_adv[,2]<-  paste0(round(r2_2,digits=2)," (",round(r2_2min,digits=2),"-",round(r2_2max,digits=2),")")
  results_table_adv[,4]<-  paste0(round(atm_2,digits=2)," (",round(atm_2min,digits=2),"-",round(atm_2max,digits=2),")")
  results_table_adv[,5]<- paste0(round(soil_2,digits=2)," (",round(soil_2min,digits=2),"-",round(soil_2max,digits=2),")")
  results_table_adv[,6]<- paste0(nb_2," (",nb_2min,"-",nb_2max,")")
  results_table_adv
 
  res_tab <- results_table_adv[order(r2_2,decreasing = T),c(1,3,2,4,5,6)]
  #require(xtable)
  print(res_tab)
  #------------------------------------------------------------------------------------------------
  # write result tables
  #------------------------------------------------------------------------------------------------
  write.csv(res_tab,file=file.path(origin,"tabs","Tab_1","Tab_1_V2.csv"))
  
  
  #------------------------------------------------------------------------------------------------
  # get mean numbers for LES and size
  #------------------------------------------------------------------------------------------------
  mean_r2 <- mean(r2_2)
  mean_r2LES <- mean(r2_2[put_into_traitGroup(results_table[,1])=="Eco"])
  mean_r2size <- mean(r2_2[put_into_traitGroup(results_table[,1])=="Size"])
  
  mean_nb <- mean(nb_2)
  mean_nbLES <- mean(nb_2[put_into_traitGroup(results_table[,1])=="Eco"])
  mean_nbsize <- mean(nb_2[put_into_traitGroup(results_table[,1])=="Size"])
  
  
  compare_soilatm <- matrix(NA,ncol=3,nrow=9)
  colnames(compare_soilatm) <- c("all","soil","atm")
  rownames(compare_soilatm) <- c("mean r2","mean relative importance Eco","mean relative importance Size",
                                 "mean nb of variables Eco", "mean nb of variables Size",
                                 "min relative importance Eco","min relative importance Size",
                                 "max relative importance Eco","max relative importance Size")
  compare_soilatm[1,] <- c(mean(r2_2),mean(soil_2),mean(atm_2))
  compare_soilatm[2,] <- c(mean(r2_2[put_into_traitGroup(results_table[,1])=="Eco"]),
                           mean(soil_2[put_into_traitGroup(results_table[,1])=="Eco"]),
                           mean(atm_2[put_into_traitGroup(results_table[,1])=="Eco"]))
  compare_soilatm[3,] <- c(mean(r2_2[put_into_traitGroup(results_table[,1])=="Size"]),
                           mean(soil_2[put_into_traitGroup(results_table[,1])=="Size"]),
                           mean(atm_2[put_into_traitGroup(results_table[,1])=="Size"]))
  compare_soilatm[4,] <- c(mean(nb_2[put_into_traitGroup(results_table[,1])=="Eco"]),
                           NA,
                           NA)
  compare_soilatm[5,] <- c(mean(nb_2[put_into_traitGroup(results_table[,1])=="Size"]),
                           NA,
                           NA)
  compare_soilatm[6,] <- c(min(r2_2[put_into_traitGroup(results_table[,1])=="Eco"]),
                           min(soil_2[put_into_traitGroup(results_table[,1])=="Eco"]),
                           min(atm_2[put_into_traitGroup(results_table[,1])=="Eco"]))
  compare_soilatm[7,] <- c(min(r2_2[put_into_traitGroup(results_table[,1])=="Size"]),
                           min(soil_2[put_into_traitGroup(results_table[,1])=="Size"]),
                           min(atm_2[put_into_traitGroup(results_table[,1])=="Size"]))
  compare_soilatm[8,] <- c(max(r2_2[put_into_traitGroup(results_table[,1])=="Eco"]),
                           max(soil_2[put_into_traitGroup(results_table[,1])=="Eco"]),
                           max(atm_2[put_into_traitGroup(results_table[,1])=="Eco"]))
  compare_soilatm[9,] <- c(max(r2_2[put_into_traitGroup(results_table[,1])=="Size"]),
                           max(soil_2[put_into_traitGroup(results_table[,1])=="Size"]),
                           max(atm_2[put_into_traitGroup(results_table[,1])=="Size"]))
  compare_soilatm
  write.csv(compare_soilatm,file=file.path(origin,"tabs","Tab_1","Tab_1_info.csv"))
  
  
}