get_number_for_manuscript <- function(){
  
  setwd("/Net/Groups")
  orig_loctem =  "BGI"#"/Volumes/BGI" 
  #orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  output_term="_sel20_"#_minmaxmed
  spec_count=20 # if output_term == "_sel20_"
  NA_now="NA_mnNbr"#"NA_stays_NA"
  
  # ---------------------------------------------------------------------------------------
  # define the origin path
  # ---------------------------------------------------------------------------------------
  
  # origin = # please add your local path here & comment the ones below.
  origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
  
  # get some functions
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_Rename_Vars3.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_put_into_traitGroup.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_traits.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_biomes.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_add_alpha.R"))
  source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","pl_plot_overlaySqunb.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","001a_prep_TRY_Env.R"))
  
  #source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","fig_4","plot_Figure_4_20191108.R"))
  #source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","fig_2","plot_Figure_2_20191108.R")) #lat gradients
  #source(file.path(origin,"scripts","Data_GapFilled","04_plot_scripts","fig_S1","plot_Figure_S1_20191108.R")) #PCA
  
  
  if(1!=1){ plot_Figure_1(origin) } # tbd
  if(1!=1){ plot_Figure_2(origin) }
  
  # ---------------------------------------------------------------------------------------
  # load data
  # ---------------------------------------------------------------------------------------
  # For Ecoregion - Scale:
  load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
                        "_aggregated_agg2",NA_now,"TRY_Env2_Kier_20191111.RData"))
  # For Grid - Scale:
  #load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
  #                    "_aggregated_agg2",NA_now,"TRY_Env2grid_Kier_20191111.RData"))
  # For Coordinate - Scale:
  #load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
  #                      "_aggregated_agg2",NA_now,"TRY_Env2coord_Kier_20191111.RData"))
  
  TRY_Env_o <- TRY_Env2_Kier
  
  # ---------------------------------------------------------------------------------------
  # process data:
  # ---------------------------------------------------------------------------------------
  # Define data set
  Appr_types = c("Data_GapFilled","Data_Predicted","Data_Observed")
  at=1
  Appr_type_now=Appr_types[at]
  #prep data:
  out <- prep_TRY_Env(Appr_type_now,output_term,Rename_Vars,TRY_Env_o,spec_count)
  
  data_master_2=out$data_master_2
  info_2=out$info_2
  trait_2=out$trait_2
  soil_2=out$soil_2
  climate_2=out$climate_2
  noise_2=out$noise_2
  climatewater_2=out$climatewater_2
  wind_2=out$wind_2
  soilwater_2=out$soilwater_2
  soiltopo_2=out$soiltopo_2
  energy_2=out$energy_2
  nutrients_2=out$nutrients_2
  
  #------------------------------------------------------------
  ##  Hierarchical Partitioning
  #------------------------------------------------------------
  list.files(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression"))
  #source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegression20191111.R"))
  source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegressionEASY_20191203.R"))
  source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","hierarchical_partitioning","hierarchical_partitioning20191204.R"))
  source(file.path(origin,"scripts",Appr_types[at],"04_plot_scripts","fig_3","plot_Figure_3_20191204.R"))
  # define data set to be loaded
  sel_now="soilAclimate"
  nruns=50
  folds=10
  ncomp_PCA=20
  do_PCA=TRUE
  output_term="_sel20_"
  
  # load analysis Ridge Regression
    sel_now="soilAclimate"
    out_now <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,firstname,secondname,nruns,output_term,ncomp_PCA,do_PCA)  
    hp_now <- hierarchical_partitioning(origin,out_now,output_term,oldData,out_rf,model_now="Ridge_Regression",trait_2,sel_now)
    
  #-----------------------------------------------------------------------
  # mean Size r2
  # mean Eco r2
  #-----------------------------------------------------------------------
    mean(colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"]))
    mean(colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
  #-----------------------------------------------------------------------
  # max Size r2
  # max Eco r2
  #-----------------------------------------------------------------------
    max(colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"]))
    which(max(colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"]))==colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"])) 
    max(colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
   which(max(colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))==colMeans(out_now$r2_soilAclimate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"])) 
   #-----------------------------------------------------------------------
   # climate r2
   #-----------------------------------------------------------------------
   min(colMeans(out_now$r2_climate))
   max(colMeans(out_now$r2_climate))
   mean(colMeans(out_now$r2_climate))
   #size
   min(colMeans(out_now$r2_climate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"]))
   max(colMeans(out_now$r2_climate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"]))
   mean(colMeans(out_now$r2_climate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"]))
   
   #Eco
   min(colMeans(out_now$r2_climate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
   max(colMeans(out_now$r2_climate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
   mean(colMeans(out_now$r2_climate[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
   
   #-----------------------------------------------------------------------
   # soil r2
   #-----------------------------------------------------------------------
   min(colMeans(out_now$r2_soil))
   max(colMeans(out_now$r2_soil))
   mean(colMeans(out_now$r2_soil))
   #size
   min(colMeans(out_now$r2_soil[,put_into_traitGroup(colnames(out_now$r2_soil))=="Size"]))
   max(colMeans(out_now$r2_soil[,put_into_traitGroup(colnames(out_now$r2_soil))=="Size"]))
   mean(colMeans(out_now$r2_soil[,put_into_traitGroup(colnames(out_now$r2_soil))=="Size"]))
   
   #Eco
   min(colMeans(out_now$r2_soil[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
   max(colMeans(out_now$r2_soil[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
   mean(colMeans(out_now$r2_soil[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"]))
   
   #-----------------------------------------------------------------------
   # independent r2
   #-----------------------------------------------------------------------
   i_s=hp_now$indep_soil-hp_now$joint_soil
   i_s[i_s<0]=0
   colMeans(i_s)*100
   mean(colMeans(i_s)*100)
   # climate size   
   max(colMeans(i_s[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"])*100)
   min(colMeans(i_s[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"])*100)
   mean(colMeans(i_s[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"])*100)
   # climate eco   
   max(colMeans(i_s[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"])*100)
   min(colMeans(i_s[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"])*100)
   mean(colMeans(i_s[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"])*100)
   
   i_c=hp_now$indep_climate-hp_now$joint_climate
   i_c[i_c<0]=0
   colMeans(i_c)*100
   max(colMeans(i_c)*100)
   min(colMeans(i_c)*100)
   mean(colMeans(i_c)*100)
   # climate size   
   max(colMeans(i_c[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"])*100)
   min(colMeans(i_c[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"])*100)
   mean(colMeans(i_c[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Size"])*100)
   # climate eco   
   max(colMeans(i_c[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"])*100)
   min(colMeans(i_c[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"])*100)
   mean(colMeans(i_c[,put_into_traitGroup(colnames(out_now$r2_soilAclimate))=="Eco"])*100)
   
   
  
   
  #------------------------------------------------------------
   ##  Hierarchical Partitioning N3
   #------------------------------------------------------------
   source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegressionEASY_20191203.R"))
   source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","hierarchical_partitioning","hierarchical_partitioning20191204.R"))
   source(file.path(origin,"scripts",Appr_types[at],"04_plot_scripts","fig_3","plot_Figure_3_20191204.R"))
   # define data set to be loaded
   sel_now="soilAclimate"
   nruns=50
   folds=10
   ncomp_PCA=20
   do_PCA=TRUE
   output_term="_sel20_"
   #output_term="sel20SoilExtend_"
   # load analysis Ridge Regression N2
   sel_now="soilAclimate"
   out_N2 <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,firstname,secondname,nruns,output_term,ncomp_PCA,do_PCA)  
   hp_now <- hierarchical_partitioning(origin,out_now=out_N2,output_term,oldData,out_rf,model_now="Ridge_Regression",trait_2,sel_now)
   
   # load analysis Ridge Regression N3
   source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","Ridge_regression","fn_load_analysis_RidgeRegressionEASYN3_20191213.R"))
   sel_now="soilwaterAAnutrientsAsoiltopo"
   sel_now="energyAAwindAclimatewater"
   
   out_N3 <- load_analysis_RidgeRegression(origin,nruns,folds,Appr_type_now,sel_now,nruns,output_term,ncomp_PCA,do_PCA)  
   
   source(file.path(origin,"scripts",Appr_types[at],"2_analysis_function","hierarchical_partitioning","hierarchical_partitioning_N3_20191213.R"))
   
   N3_res <- matrix(NA,ncol=3,nrow=17)
   rownames(N3_res) <- colnames(out_N2[[1]])
   colnames(N3_res) <- c(names(out_N3)[1:3])
   
   i=1
   for(i in 1:17){
     X0=0
     X1=mean(out_N3[[1]][,i])
     X2=mean(out_N3[[2]][,i])
     X3=mean(out_N3[[3]][,i])
     X12=mean(out_N3[[4]][,i])
     X13=mean(out_N3[[5]][,i])
     X23=mean(out_N3[[6]][,i])
     if(sel_now=="soilwaterAAnutrientsAsoiltopo"){X123=mean(out_N2[[1]][,i])}
     if(sel_now=="energyAAwindAclimatewater"){X123=mean(out_N2[[2]][,i])}
     hp <- hierarchical_partitioning_N3(X0,X1,X2,X3,X12,X13,X23,X123)
     N3_res[i,] <- c(hp[[1]],hp[[2]],hp[[3]])
   }
   
   colMeans(N3_res)
   
}