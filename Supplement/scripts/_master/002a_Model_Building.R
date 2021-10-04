model_analyses <- function(origin, output_term, doPCA,
                           doRR,    doLM,    doRF,    doPLS,
                           nruns_RR,nruns_LM,nruns_RF,nruns_PLS){
  #install.packages("dplyr")
  # ------------
  # load data
  # ------------
  load(file = file.path(origin,"data","master_matrix",paste0("X2_",output_term,"_NEE.RData")))
  
   info=TRY_Env$info
   trait=TRY_Env$trait
   if(output_term=="PCA"){
     require(FactoMineR)
     pca_trait=PCA(log(trait))
     trait <- pca_trait$ind$coord[,1:3]
   }
   
   soil = TRY_Env$soil
   climate = TRY_Env$climate
   latitude = abs(cbind(TRY_Env$info$Lat,TRY_Env$info$max.lat,TRY_Env$info$min.lat))
   if(output_term=="climLat"){soil = latitude}
   if(output_term=="soilLat"){climate = latitude}
   noise = TRY_Env$noise
   climate_energy = TRY_Env$climate_energy
   climate_water = TRY_Env$climate_water
   soil_physics=TRY_Env$soil_physics
   soil_chemistry=TRY_Env$soil_chemistry

   head(trait)
  #---------------------------------------------------------------------------------------
  # ANALYSES
  # LM 
  #----------------------------------------------------------------------------------------
  
  if(doLM&(output_term==""|output_term=="obs"|output_term=="PCA")){
    if(output_term==""){print("do lm for Supplementary Figure 15.")}
    if(output_term=="obs"){print("do lm for Supplementary Figure 16.")}
    if(output_term=="PCA"){print("do lm for Supplementary Figure 34-37.")}
    
    nruns = nruns_LM # number of repetitions for linear models
    # creating random chunks of rows that enter the 10-fold cross validation 
    # per repetition one
    folds=10
    chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
    chunk_list <- list()
    for(i in 1:nruns){
      chunk_list[[i]] <- chunk(1:nrow(trait),folds)
      names(chunk_list[[i]] ) <- 1:folds
    }
    
    climOsoils=c("climate","soil","soilAclimate")#
    climOsoil="soilAclimate"
    if(output_term=="PCA"){climOsoils=c("soilAclimate")}
    ncomp_PCA=20
    for(climOsoil in climOsoils){
      print(climOsoil)
      result_lm <- analysis_lm(climate,soil=soil,trait,
                                 origin,climOsoil,folds,nruns,
                                 output_term,chunk_list,doPCA,ncomp_PCA) 
    }
  }else{
    print("No linear model.")}
  
  #----------------------------------------------------------------------------------------
  # Ridge Regression
  #----------------------------------------------------------------------------------------
  if(doRR){
    print("do Ridge Regression")
    
    nruns = nruns_RR # number of repetitions for ridge regression
    # creating random chunks of rows that enter the 10-fold cross validation 
    # per repetition one
    folds=10 # number of folds for cross validation 
    chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
    chunk_list <- list()
    for(i in 1:nruns){
      chunk_list[[i]] <- chunk(sample(1:nrow(trait)),folds)
    }

    #define 
    if(output_term==""){
      climOsoils=c(
      # for the original run
      "soil","climate","soilAclimate",
      # for the noise run (supplementary): 
      "noise","noiseAclimate","soilAnoise",
      # for the single trait runs (figure S4n):
      "soil_physics","soil_chemistry",
      "climate_water","climate_energy")}
    if(output_term!=""){
      climOsoils=c("climate","soil","soilAclimate")
    if(output_term=="PCA"){
        climOsoils=c(
          # for the original run
          "soil","climate","soilAclimate",
          # for the single trait runs (figure S4n):
          "soil_physics","soil_chemistry",
          "climate_water","climate_energy")
        }
    }
    
    
    ncomp_PCA=20


    try(colSums(!is.na(trait)))
    
    climOsoil="soil"
    for(climOsoil in climOsoils){
      print(climOsoil)
      result_ridge <- analysis_RidgeRegression(climate,soil,trait,
                                               noise,
                                               climate_energy,climate_water,
                                               soil_chemistry,soil_physics,
                                               origin,Appr_type_now,climOsoil,
                                               nruns,output_term,chunk_list,ncomp_PCA,folds,doPCA)
    }
  }else{
    print("No Ridge Regression.")}   
  
  #---------------------------------------------------------------------------------------
  # PLS 
  #----------------------------------------------------------------------------------------
  if(doPLS & output_term==""){
    print("do PLS for supplementary")
    print(paste0("PCA: ",doPLS))
    
    nruns = nruns_PLS # number of repetitions for partial least squares (PLS)
    # creating random chunks of rows that enter the 10-fold cross validation 
    # per repetition one
    folds=10
    chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
    chunk_list <- list()
    for(i in 1:nruns){
      chunk_list[[i]] <- chunk(sample(1:nrow(trait)),folds)
    }
    
    require(foreach)
    require(doParallel)
    registerDoParallel(cores = 3)
    
    climOsoils=c("climate","soil","soilAclimate")
    for(climOsoil in climOsoils){
      print(climOsoil)
      result_pls <- analysis_pls(climate,soil,trait,
                                 origin,climOsoil,folds,nruns,
                                 output_term,chunk_list,doPCA,ncomp_PCA=20) 
    }
  }else{
    print("No PLS.")}   
  
  
  #----------------------------------------------------------------------------------------
  # Random Forest 
  #----------------------------------------------------------------------------------------
  if(doRF & output_term==""& !doPCA){
    
    nruns = nruns_RF # number of repetitions for random forest
    # creating random chunks of rows that enter the 10-fold cross validation 
    # per repetition one
    folds=10
    chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
    chunk_list <- list()
    for(i in 1:nruns){
      chunk_list[[i]] <- chunk(sample(1:nrow(trait)),folds)
    }
    
    climOsoils=c("climate","soil","soilAclimate")
    for(climOsoil in climOsoils){
      print(climOsoil)
      result_rf <- analysis_RandomForest(climate,soil,trait,origin,
                                  climOsoil,nruns,folds,output_term,chunk_list = chunk_list,ncomp_PCA=20) 
    }
  }else{
    print("No Random Forest.")}   
    
    
}
