Tab_S4 <- function(origin,Agg_type, sel_now){
  
  meanOsd="median"
  atmObio="soilAatmosphere"
  Agg_type="Agg2lin_NULL_"
  
  #------------------------------------------------------------------------------------------------
  # load test & get r2 rel importances
  #------------------------------------------------------------------------------------------------
  load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),paste0(atmObio,"_trait_",meanOsd,"test_",Agg_type,"1",".RData")))
      test_smr <- test
  
  r2_2 <- matrix(NA,nrow=length(test_smr$accuracy),ncol=3)
  colnames(r2_2) <- c("mean r2","5th quantile r2","95th quantile r2")
  rownames(r2_2) <- rename_for_plot(gsub(test$colnames_resp,pattern = meanOsd,replacement = ""))
  for(t in 1:length(test_smr$accuracy)){
    r2_2[t,1] <- round(mean(test_smr$accuracy[[t]][,2]),digits = 3)
    r2_2[t,2] <- round(quantile(test_smr$accuracy[[t]][,2],probs = 0.05),digits = 3)
    r2_2[t,3] <- round(quantile(test_smr$accuracy[[t]][,2],probs = 0.95),digits = 3)
    }
  
  print(r2_2)
  write.csv(r2_2,file=file.path(origin,"tabs","Tab_S4",paste0("NULL_table_nruns",nruns,".csv")))
  
}
