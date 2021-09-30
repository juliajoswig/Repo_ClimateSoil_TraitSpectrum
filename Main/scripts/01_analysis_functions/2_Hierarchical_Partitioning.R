hierarchical_partitioning00 <- function(
  origin,
  nruns,doPCA){



#------------------------------------------------------------
##  Hierarchical Partitioning 
#------------------------------------------------------------
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_wrapper.R"))
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_inner.R"))
  source(file.path(origin,"scripts","01_analysis_functions","RidgeRegression","fn_easyload_RR.R"))
  
#-------------
# load analysis Ridge Regression N2
#-------------
  climOsoil="soilAclimate"
    r2_l <- list()
    hp_l <- list()
    counter=0
    print("---")
    print(climOsoil)
    counter=1+counter
          
    r2_l[[counter]] <- load_analysis(origin,nruns,climOsoil,doPCA)
    names(r2_l)[[counter]] <- climOsoil
    hp_l[[counter]] <- hierarchical_partitioning(origin,output_term,climOsoil,out_now = r2_l[[counter]])
    names(hp_l)[[counter]] <- climOsoil
  
#-------------
# save
#-------------
      
    if(!file.exists(file.path(origin, "data", "_results","HierarchicalPartitioning"))){
      dir.create(file.path(origin, "data", "_results","HierarchicalPartitioning"))}
    if(!file.exists(file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results"))){
        dir.create(file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results"))}
        if(doPCA){pca_term="doPCA"}
        if(!doPCA){pca_term="no_PCA"}
        save(hp_l,file=file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results",
                                 paste0("HP_",nruns,"nruns_",pca_term,".RData")))
        save(r2_l,file=file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results",
                                 paste0("R2_",nruns,"nruns_",pca_term,".RData")))

}
