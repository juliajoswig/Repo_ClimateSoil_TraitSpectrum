total_hierarchical_partitioning_analyses <- function(
  origin,output_term,
  doPLS,doPCA,doRR,doRF,
  nruns_RR,nruns_LM,nruns_RF,nruns_PLS){
  

  if((doRR  &(!doRF &!doPLS))|
     (doPLS &(!doRR &!doRF))|
     (doRF &(!doPLS&!doRR))){


#------------------------------------------------------------
##  Hierarchical Partitioning & Figures 3a 3b
#------------------------------------------------------------
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_wrapper.R"))

  if(doRR){source(file.path(origin,"scripts","01_analysis_functions","RidgeRegression","fn_easyload_RR.R"))
    nruns=nruns_RR}
  if(doRF){source(file.path(origin,"scripts","01_analysis_functions","RandomForest","fn_easyload_RandomForest.R"))
    nruns=nruns_RF}
  if(doPLS){source(file.path(origin,"scripts","01_analysis_functions","PLS","fn_easyload_PLS.R"))
    nruns=nruns_PLS}

#-------------
# load analysis Ridge Regression N2
#-------------
  # define data set to be loaded
  if(!doRR){
    climOsoils = c("soilAclimate")
    output_terms = c("")
  }else{
    output_terms=c(
      "", # original input data
      "woody","non_woody", # input data includes only woody or non-woody species
      "obs", # input data not with gap-filled, but observed data
      "grid", # input data not aggregated to ecoregions, but to grids
      "NOsel", # input data/ Ecoregions without selection criteria
      "PCA", # input traits as their first PCs from PCA
      "climLat","soilLat", # climate or soil variables replaced with latitudinal information
      "bias1","bias2","bias3" # traits randomized across the X0 (individual plants) level, then aggregated to ecoregions
    ) 
    climOsoils=c("soilAclimate",
                 "noiseAclimate","soilAnoise",
                 # for the single trait runs (figure S3 - figure S20):
                 "soil_chemistryAsoil_physics",
                 "climate_waterAclimate_energy"
    )
    
  }
    output_term="non_woody"
    for(output_term in output_terms){
      r2_l <- list()
      hp_l <- list()
      counter=0
    for(climOsoil in climOsoils){
      print("---")
      print(climOsoil)
      print(output_term)

        
      if((output_term==""&climOsoil%in%c("soilAclimate","soilAnoise","noiseAclimate","soil_chemistryAsoil_physics","climate_waterAclimate_energy"))|
           (output_term=="woody"&climOsoil%in%c("soilAclimate"))|(
             output_term=="NOsel"&climOsoil%in%c("soilAclimate"))|(
               output_term=="obs"&climOsoil%in%c("soilAclimate"))|(
                 output_term=="bias1"&climOsoil%in%c("soilAclimate"))|(
                 output_term=="bias2"&climOsoil%in%c("soilAclimate"))|(
                 output_term=="bias3"&climOsoil%in%c("soilAclimate"))|(
                   output_term=="biome"&climOsoil%in%c("soilAclimate"))|(
                     output_term=="grid"&climOsoil%in%c("soilAclimate"))|(
                         output_term=="PCA"&climOsoil%in%c("soilAclimate","soil_chemistryAsoil_physics","climate_waterAclimate_energy"))|(
                           output_term=="climLat"&climOsoil%in%c("soilAclimate"))|(
                             output_term=="soilLat"&climOsoil%in%c("soilAclimate"))|(
                           output_term=="non_woody"&climOsoil%in%c("soilAclimate"))){
          
          counter=1+counter

          
          r2_l[[counter]] <- load_analysis(origin,nruns,climOsoil,output_term,doPCA)
          names(r2_l)[[counter]] <- climOsoil
          if(output_term=="noPCA"){
            r2_l[[counter]][[1]] <- r2_l[[counter]][[1]][,1:17]
            r2_l[[counter]][[2]] <- r2_l[[counter]][[2]][,1:17]
            r2_l[[counter]][[3]] <- r2_l[[counter]][[3]][,1:17]
          }
          hp_l[[counter]] <- hierarchical_partitioning(origin,output_term,climOsoil,out_now = r2_l[[counter]])
          names(hp_l)[[counter]] <- climOsoil
        }

    }
      #-------------
      # save
      #-------------
      
      if(doRR){
        if(!file.exists(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results"))){
          dir.create(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results"))}
        if(doPCA){pca_term="doPCA"}
        if(!doPCA){pca_term="no_PCA"}
        save(hp_l,file=file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                                 paste0("HP_",nruns,"nruns_",output_term,pca_term,".RData")))
        save(r2_l,file=file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                                 paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
      }
      if(doPLS){
        if(doPCA){pca_term="doPCA"}
        if(!doPCA){pca_term="no_PCA"}
        if(!file.exists(file.path(origin, "data", "_results","PLS","_PLS_results"))){
          dir.create(file.path(origin, "data", "_results","PLS","_PLS_results"))}
        save(hp_l,file=file.path(origin, "data", "_results","PLS","_PLS_results",paste0("HP_",nruns,"nruns_",output_term, pca_term,".RData")))
        save(r2_l,file=file.path(origin, "data", "_results","PLS","_PLS_results",paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
      }
      if(doRF){
        if(!file.exists(file.path(origin, "data", "_results","RandomForest","_RandomForest_results"))){
          dir.create(file.path(origin, "data", "_results","RandomForest","_RandomForest_results"))}
        save(hp_l,file=file.path(origin, "data", "_results","RandomForest","_RandomForest_results",paste0("HP_",nruns,"nruns_",output_term,".RData")))
        save(r2_l,file=file.path(origin, "data", "_results","RandomForest","_RandomForest_results",paste0("R2_",nruns,"nruns_",output_term,".RData")))
      }


  }

  
  }

}
