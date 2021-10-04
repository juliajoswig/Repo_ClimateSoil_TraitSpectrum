individual_trait_values <- function(origin,atmObio,meanOsd,nruns,Agg_type,sel_now){
  
  trait.names[5] <- "LeN"
  # ---------------------------  
  #  correlation coefficients
  # ---------------------------  
  load(file.path(origin, "data","Aggregations","Spatial_Aggregations","dat_cor.RData"))
  trait_cor <- abs(dat_cor)
  
  # ---------------------------  
  #  load test data
  # ---------------------------  
  # load test data
  load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),paste0(atmObio,"_trait_",meanOsd,"test_",Agg_type,sel_now,".RData")))
  load(file.path(origin,"data","input","predAresp",paste0("resp_","DC_",atmObio,"_and_Trait_",meanOsd,"_",Agg_type,sel_now,".RData")))
  load(file.path(origin,"data","input","predAresp",paste0("pred_","DC_",atmObio,"_and_Trait_",meanOsd,"_",Agg_type,sel_now,".RData")))
  r2 <- test$rel_I_metrics[,11]
  #match(trait.names,gsub(rownames(test$rel_I_metrics),pattern = "_median",replacement = ""))
  #match(gsub(rownames(test$rel_I_metrics),pattern = "_median",replacement = ""),trait.names)
  r2 <- r2[match(trait.names,gsub(rownames(test$rel_I_metrics),pattern = "_median",replacement = ""))]
  # ---------------------------  
  #  number of variables
  # ---------------------------  
  nb_pred <- test$rel_I_metrics[,16]
  nb_pred <-  nb_pred[match(trait.names,gsub(rownames(test$rel_I_metrics),pattern = "_median",replacement = ""))]
  
  rel_importance <- test$df_final2
  # ---------------------------  
  #  r2 min and max
  # ---------------------------  
  min_r2 <- test$rel_I_metrics[,14]
  max_r2 <- test$rel_I_metrics[,15]
  
  # ---------------------------  
  #  rel impo soil min and max
  # ---------------------------  
  min_soil <- test$rel_I_metrics[,9]*test$rel_I_metrics[,11]
  max_soil <- test$rel_I_metrics[,10]*test$rel_I_metrics[,11]
  # ---------------------------  
  #  trait group
  # --------------------------- 
  
  print(paste0( 
    "Results of single variable selection outputs (n=100, CARS-PLS, see methods Variable selection) to explain individual traits",". a) The ",
    "fraction of explained variance by soil or climate variables called importance split into ",
    "x-axis = the cumulative importance of soil",
    " and the y-axis = cumulative importance of climate variables. ",
    "Each point represents a model output; overplotting results in less visible points.",
    " b) This riverplot depicts the single variables selected, including the relative importance for explaining individual traits",
    ". On the left side sit all selected variables (table S2), thickness of bars indicates their relative importance, the",
    " colour of the bar indicates a positive (orange) or negative (green) correlation coefficient. ",
    "In the centre of the riverplot sit the categories of varibles, the bars thickness indicates their variables' cumulative importance, ",
    "categories' colouring indicates the attribution to soil (light red) or climate (light blue) variables. "))
  
  t=3
  n=0
 for(t in order(r2,decreasing = T)){
   n=n+1
   print(" ------------------------------- ")
   print(rownames(dat_cor)[t])
   print(" ------------------------------- ")
   
  if(abs(r2[t])>.6){ix=3};  if(abs(r2[t])<=.6){ix=2};  if(abs(r2[t])<.4){ix=1}
   if(nb_pred[t] >= quantile(nb_pred,probs = .3)){ix2=2}; if(nb_pred[t] > quantile(nb_pred,probs = .7)){ix2=1}
   if(nb_pred[t] < quantile(nb_pred,probs = .3)){ix2=3}
   if(max_r2[t]-min_r2[t] > quantile(max_r2-min_r2,probs = .3)){ix4=2}; if(max_r2[t]-min_r2[t] > quantile(max_r2-min_r2,probs = .7)){ix4=1}
   if(max_r2[t]-min_r2[t] < quantile(max_r2-min_r2,probs = .3)){ix4=3}
   if(max_soil[t]-min_soil[t] > quantile(max_soil-min_soil,probs = .3)){ix5=2}; if(max_soil[t]-min_soil[t] > quantile(max_soil-min_soil,probs = .7)){ix5=1}
   if(max_soil[t]-min_soil[t] < quantile(max_soil-min_soil,probs = .3)){ix5=3}
   
  ixa <- order(as.numeric(trait_cor[t,]), decreasing = T)[2]
  ixb <- order(trait_cor[t,], decreasing = T)[3]
  ixc <- order(trait_cor[t,], decreasing = T)[4]
  
  # get relative importance
  trait_now <- rel_importance[rel_importance$trait%in%trait.names[t],]
  ix3 = order(as.numeric(as.vector(unlist(trait_now$rel_Importance))),decreasing = TRUE)
  # get most important environmental variables
  trait_now$new_driver <- paste0(rename_what_can_be_renamed(trait_now$driver),"_",rename_what_can_be_renamed(trait_now$relationship))
 
  print(paste0( 
                rownames(dat_cor)[t]," is a ", put_into_traitGroup(trait.names[t])," trait, which is ",
                c("poorly","medium well","well")[ix], " explained"," (",round(r2[t],digits = 2)*100,"%" ,", table 1, figure 3a).",
                " It correlates most with ", 
                colnames(trait_cor)[ixa]," (",put_into_traitGroup(trait.names[ixa]),", cor. coeff.: ", round(dat_cor[t,ixa],digits = 2),", figure 1)",", ",
                colnames(trait_cor)[ixb]," (",put_into_traitGroup(trait.names[ixb]),", cor. coeff.: ", round(dat_cor[t,ixb],digits = 2) ,", figure 1)"," and ",
                colnames(trait_cor)[ixc]," (",put_into_traitGroup(trait.names[ixc]),", cor. coeff.: ", round(dat_cor[t,ixc],digits = 2),", figure 1). ",
                rename_for_plot(trait.names[t])," needs ",c("many","an average number of","few")[ix2],
                " variables per model ","(nb_mean = ",nb_pred[t],")." ,
                c(paste0(" The model stability is poor, as the explained variance ranges from ",round(min_r2[t]*100,digits=0),"% to ", round(max_r2[t]*100,digits=0),"%."),
                  paste0(" The model stability is average: the explained variance ranges from ",round(min_r2[t]*100,digits=0),"% to ", round(max_r2[t]*100,digits=0),"%."),
                  paste0(" The model stability is high, as the explained variance ranges from ",round(min_r2[t]*100,digits=0),"% to ", round(max_r2[t]*100,digits=0),"%."))[ix4],
                c(paste0(" The stability of the soil vs climate signal is poor, the relative importance ranges from ",round(min_soil[t]*100,digits=0),"% to ", round(max_soil[t]*100,digits=0),"% figure S3.",n,"a)."),
                  paste0(" The stability of the soil vs climate signal is average, the relative importance ranges from ",round(min_soil[t]*100,digits=0),"% to ", round(max_soil[t]*100,digits=0),"% figure S3.",n,"a)."),
                  paste0(" The stability of the soil vs climate signal is high, the relative importance ranges from ",round(min_soil[t]*100,digits=0),"% to ", round(max_soil[t]*100,digits=0),"% figure S3.",n,"a)."))[ix5],
                " The most important environmental variables for this ",put_into_traitGroup(trait.names[t])," trait, are ",
                trait_now$new_driver[ix3][1],
                " (",put_into_soilOrAtm2(trait_now$driver[ix3][1]),
                ", rel_I=",round(as.numeric(as.vector(unlist(trait_now$rel_Importance)))[ix3[1]]*100,digits=1),"%, table S5), ",
                trait_now$new_driver[ix3][2],
                " (",put_into_soilOrAtm2(trait_now$driver[ix3][2]),
                ", rel_I=",round(as.numeric(as.vector(unlist(trait_now$rel_Importance)))[ix3[2]]*100,digits=1),"%, table S5) and ",
                trait_now$new_driver[ix3][3],
                " (", put_into_soilOrAtm2(trait_now$driver[ix3][3]),
                ", rel_I=",round(as.numeric(as.vector(unlist(trait_now$rel_Importance)))[ix3[3]]*100,digits=1),"%, table S5). "))


 }

  
  }
par(mar=c(4,4,1,1),mfrow=c(1,3))
plot(pred$CLYPPT_M_sl1_1km_ll.tif_mean,pred$SNDPPT_M_sl1_1km_ll.tif_mean)
plot(pred$CLYPPT_M_sl1_1km_ll.tif_mean,pred$SLTPPT_M_sl1_1km_ll.tif_mean)
plot(pred$SNDPPT_M_sl1_1km_ll.tif_mean,pred$SLTPPT_M_sl1_1km_ll.tif_mean)

summary(pred$SLTPPT_M_sl1_1km_ll.tif_mean)
