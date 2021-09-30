fn_NULL_infos <- function(){
  
  xi=4
  yi=1
  r2_smr <- matrix(NA,nrow=length(trait.names),ncol=length(n_option));rownames(r2_smr) <- trait.names
  trait.names[5] <- "LeN"
  # load NULLs
  for(sel_now in 1:5){
    atmObio = Xs[xi]
    meanOsd = Ys[yi]
    
    load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),"summary",paste0(atmObio,"_trait_",meanOsd,"_",Agg_type,sel_now,"_RiverInput_r2weighted.RData")))
    
    for(t in 1:length(trait.names)){
      r2_smr[t,sel_now] <- unique(as.vector(unlist(df_final2$r2[df_final2$trait%in%trait.names[t]])))
    }
  }
  
  r2_smr <- r2_smr[which(rownames(r2_smr)!="LDMC"),]
  # r2 per trait
  quantile(as.numeric(r2_smr),probs = 0.95)
  quantile(as.numeric(r2_smr),probs = 0.05)
  mean(as.numeric(r2_smr))
  sd(as.numeric(r2_smr))

  require(xtable)
  
  xtable(r2_smr)
  
  
}