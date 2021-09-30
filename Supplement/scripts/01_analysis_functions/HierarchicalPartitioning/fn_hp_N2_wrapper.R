hierarchical_partitioning <- function(origin,output_term,climOsoil,out_now){
  
  out=out_now

  X1 <-  out[[paste0("r2_",substring(text = climOsoil,first = 1,last = regexpr("A", climOsoil)[1]-1))]]
  X2 <- out[[paste0("r2_",sub(".*A", "", climOsoil))]]
  X12 <- out[[paste0("r2_",climOsoil)]] 

  if(sum(is.na(X1))!=0){X1[is.na(X1)] <-0}
  if(sum(is.na(X2))!=0){X2[is.na(X2)] <- 0}
  if(sum(is.na(X12))!=0){X12[is.na(X12)] <- 0}
  
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_inner.R"))
  
  hp_N2 <- hierarchical_partitioning_N2(X0,X1,X2,X12,climOsoil)

  return(hp_N2)
  
}
