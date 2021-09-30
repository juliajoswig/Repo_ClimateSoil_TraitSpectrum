get_the_values <- function(meanOsd){
  
  
  # better check script Tab_1.R
  meanOsd="median"
  load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),paste0("soilAatmosphere_trait_mediantest_Agg2lin_.RData")))
    #load test
  load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),paste0("soilAatmosphere_trait_mediantest_Agg2lin_.RData")))
  
  test$rel_I_metrics
  
  r2_2 <- r2[2:nrow(r2),which(substr(colnames(r2),start=1,stop=nchar(meanOsd))%in%meanOsd)][,1]
  soil_2 <- soil[2:nrow(soil),which(substr(colnames(soil),start=1,stop=nchar(meanOsd))%in%meanOsd)][,1]
  atm_2 <-  atm[2:nrow(atm),which(substr(colnames(atm),start=1,stop=nchar(meanOsd))%in%meanOsd)][,1]
  
  #no LDMC
  r2_2 <- r2_2[rownames(as.matrix(r2_2))!="LDMC"]
  soil_2 <- soil_2[rownames(as.matrix(soil_2))!="LDMC"]
  atm_2 <-  atm_2[rownames(as.matrix(atm_2))!="LDMC"]
  
  maximum_r2 <- max(r2_2)  
  maximum_r2 <- c(rownames(r2)[2:nrow(r2)][which(maximum_r2==r2_2)],maximum_r2)
  mean_r2_tot <- mean(r2_2)  
  mean_r2_atm <- mean(atm_2)  
  sd_r2_atm <- sd(atm_2)  
  mean_r2_soil <- mean(soil_2)  
  sd_r2_soil <- sd(soil_2)  
  
  
  ix_LES=which(put_into_traitGroup(rownames(as.matrix(r2_2)))=="LES")
  maximum_r2_LES <- max(r2_2[ix_LES])  
  maximum_r2_LES <- c(rownames(as.matrix(r2_2))[ix_LES][which(maximum_r2_LES==r2_2[ix_LES])],maximum_r2_LES)
  mean_r2_tot_LES <- mean(r2_2[ix_LES])  
  mean_r2_atm_LES <- mean(atm_2[ix_LES])  
  sd_r2_atm_LES <- sd(atm_2[ix_LES])  
  mean_r2_soil_LES <- mean(soil_2[ix_LES])  
  sd_r2_soil_LES <- sd(soil_2[ix_LES])  
  
  
  ix_SIZE=which(put_into_traitGroup(rownames(as.matrix(r2_2)))=="size")
  maximum_r2_Size <- max(r2_2[ix_SIZE])  
  maximum_r2_Size <- c(rownames(as.matrix(r2_2))[ix_SIZE][which(maximum_r2_Size==r2_2[ix_SIZE])],maximum_r2_Size)
  mean_r2_tot_Size <- mean(r2_2[ix_SIZE])  
  mean_r2_atm_Size <- mean(atm_2[ix_SIZE])  
  sd_r2_atm_Size <- sd(atm_2[ix_SIZE])  
  mean_r2_soil_Size <- mean(soil_2[ix_SIZE])  
  sd_r2_soil_Size <- sd(soil_2[ix_SIZE])  
  
  if(sum(trait.names%in%"LDMC")==1){trait.names <- trait.names[-which(trait.names%in%"LDMC")]}
  results_table <- matrix(NA,ncol=6,nrow=length(trait.names));colnames(results_table)=c("trait","r2","group","rel_I climate","rel_I soil","nb_pred")
  rownames(results_table) <- trait.names
  
  trait.names[5] <- "LeN_"
  results_table[,1] <- trait.names
  results_table[,2] <- round(r2_2[which(rownames(as.matrix(r2_2))%in%trait.names)],digits = 3)
  results_table[,3] <- put_into_traitGroup(trait.names)
  results_table[,4] <- round(atm_2[which(rownames(as.matrix(atm_2))%in%trait.names)],digits = 3)
  results_table[,5] <- round(soil_2[which(rownames(as.matrix(soil_2))%in%trait.names)],digits = 3)
  
  rownames(results_table)[5] <- "LeN"
  results_table_adv <- results_table
  for(t in 1:nrow(results_table_adv)){
    ti=which(gsub(test$colnames_resp,pattern = paste0("_",meanOsd),replacement = "")==rownames(results_table)[t])
    results_table_adv[t,2]<- paste0(results_table_adv[t,2]," (",round(quantile(test$accuracy[[ti]][,2],probs = 0.05),digits=3),"-",round(quantile(test$accuracy[[ti]][,2],probs = 0.95),digits=3),")")
    results_table_adv[t,6]<- paste0(round(mean(test$accuracy[[ti]][,4]),digits=1)," (",round(quantile(test$accuracy[[ti]][,4],probs = 0.05),digits=1),"-",round(quantile(test$accuracy[[ti]][,4],probs = 0.95),digits=1),")")
    
    results_table_adv[t,4]<- paste0(round(mean(test$rel_I_atmWeighted[[ti]]),digits=3)," (",round(quantile(test$rel_I_atmWeighted[[ti]],probs = 0.05),digits=3),"-",round(quantile(test$rel_I_atmWeighted[[ti]],probs = 0.95),digits=3),")")
    results_table_adv[t,5]<- paste0(round(mean(test$rel_I_soilWeighted[[ti]]),digits=3)," (",round(quantile(test$rel_I_soilWeighted[[ti]],probs = 0.05),digits=3),"-",round(quantile(test$rel_I_soilWeighted[[ti]],probs = 0.95),digits=3),")")
  }
  require(xtable)
  xtable(results_table_adv)
  
  t=1
  nb_pred_LES <- NA;nb_pred_tot <- NA;nb_pred_size <- NA
  for(t in c(ix_SIZE,ix_LES)){ ti=which(gsub(test$colnames_resp,pattern = paste0("_",meanOsd),replacement = "")==rownames(results_table)[t])
  nb_pred_tot <- c(nb_pred_tot,mean(test$accuracy[[ti]][,4]))}
  for(t in ix_LES){ ti=which(gsub(test$colnames_resp,pattern = paste0("_",meanOsd),replacement = "")==rownames(results_table)[t])
  nb_pred_LES <- c(nb_pred_LES,mean(test$accuracy[[ti]][,4]))}
  for(t in ix_SIZE){ ti=which(gsub(test$colnames_resp,pattern = paste0("_",meanOsd),replacement = "")==rownames(results_table)[t])
  nb_pred_size <- c(nb_pred_size,mean(test$accuracy[[ti]][,4]))}
  nb_pred_tot<- nb_pred_tot[!is.na(nb_pred_tot)]
  nb_pred_LES<- nb_pred_LES[!is.na(nb_pred_LES)]
  nb_pred_size<- nb_pred_size[!is.na(nb_pred_size)]
  mean(nb_pred_tot)
  mean(nb_pred_size)
  mean(nb_pred_LES)
  
  out <- list(
    # total (LES and Size)
    maximum_r2=maximum_r2,
    mean_r2_tot=mean_r2_tot,
    mean_r2_atm=mean_r2_atm,
    sd_r2_atm=sd_r2_atm,
    mean_r2_soil=mean_r2_soil,
    sd_r2_soil=sd_r2_soil,
    # LES            
    maximum_r2_LES=maximum_r2_LES,
    mean_r2_tot_LES=mean_r2_tot_LES,
    mean_r2_atm_LES=mean_r2_atm_LES,
    sd_r2_atm_LES=sd_r2_atm_LES,
    mean_r2_soil_LES=mean_r2_soil_LES,
    sd_r2_soil_LES=sd_r2_soil_LES,
    # Size
    maximum_r2_Size=maximum_r2_Size,
    mean_r2_tot_Size=mean_r2_tot_Size,
    mean_r2_atm_Size=mean_r2_atm_Size,
    sd_r2_atm_Size=sd_r2_atm_Size,
    mean_r2_soil_Size=mean_r2_soil_Size,
    sd_r2_soil_Size=sd_r2_soil_Size,
    
    results_table = results_table,
    results_table_adv = results_table_adv
  )
  
  return(out)
  
}