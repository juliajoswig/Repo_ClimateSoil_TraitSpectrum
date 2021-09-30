build_RR_models <- function(origin, # file path
                                 output_term, # defines which subset (total, woody, non-woody)
                                 nruns,# number of model building repetitions for ridge regression final version = 50, for quick version nruns=2
                                 doPCA # logical. defines if prior to the ridge regression, PCA will be performed on soil&climate variables
){
  packages_loaded <- rownames(installed.packages())
  if(sum(packages_loaded=="dplyr")<1){install.packages("dplyr")}
  if(sum(packages_loaded=="glmnet")<1){install.packages("glmnet")}
  
  #---------------------------------------------------------------------------------------
  # load data
  #---------------------------------------------------------------------------------------
  load(file = file.path(origin,"data","master_matrix",paste0("X2.RData")))
  
   trait=TRY_Env$trait
   soil = TRY_Env$soil
   climate = TRY_Env$climate
   latitude = abs(cbind(TRY_Env$info$Lat,TRY_Env$info$max.lat,TRY_Env$info$min.lat))

   head(trait)
  #---------------------------------------------------------------------------------------
  # ANALYSES
  # Ridge Regression
  #----------------------------------------------------------------------------------------

    print("Doing Ridge Regression.")
    ncomp_PCA=20
    folds=10 # number of folds for cross validation 
    chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
    chunk_list <- list()
    for(i in 1:nruns){
      chunk_list[[i]] <- chunk(sample(1:nrow(trait)),folds)
    }

      climOsoils=c(# for the original run
      "soil","climate","soilAclimate")
      
    source(file.path(origin,"scripts","01_analysis_functions","RidgeRegression","fn_RidgeRegression.R"))
    for(climOsoil in climOsoils){
      print(climOsoil)
      result_ridge <- analysis_RidgeRegression(climOsoil,climate,soil,trait,nruns,chunk_list,ncomp_PCA,folds,doPCA)
    }

}
