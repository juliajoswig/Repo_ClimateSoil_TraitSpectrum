# Masterscript accompanying the manuscript 
# Climatic and soil factors explain the two-dimensional spectrum of global plant trait variation
# Joswig et al. NEE
# by Julia Joswig email: julia.joswig@geo.uzh.ch (or juliajoswigjj@gmail.com)
# ------------

# 00 - analysis path
# 1 - Ridge Regression - model building 
# 2 - hierarchical partitioning analyses
# 3 - create the figures
# 4 - create the tables
# 5 - get numbers


# ---------------------------------------------------------------------------------------
# 00 - define the origin path
# ---------------------------------------------------------------------------------------
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Main" 
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/Repo_ClimateSoil_TraitSpectrum/Main" 
#  origin =  # please input your directory here
  list.files(origin)
  
# load functions from functions scripts
  source(file.path(origin,"scripts" ,"_support_functions","fn_functions.R"))
# Errors: load table_1.R no working 
  source(file.path(origin,"scripts" ,"_support_functions","fn_packages.R"))# check, this does not exist.


#------------------------------------------------------------------------------------
# [1.] Ridge Regression - model building 
# input folder: data/master_matrix
# output folder: data/_results (output files for nruns=50 already available)
#------------------------------------------------------------------------------------
    nruns = 50 # number of model building repetitions for ridge regression final version = 50, for quick version nruns=2
    doPCA = TRUE # define if hierarchichal paritioning for PCA prior to RR or PLS is to be done analysis to be done
    build_RR_models(origin, output_term,  nruns, doPCA)

#------------------------------------------------------------------------------------
# [2.] hierarchical partitioning analyses
# input folder:  data/_results/RR/(selected variables)/nruns/
# output folder: data/_results/RR/_(model type)_results
#------------------------------------------------------------------------------------
    hierarchical_partitioning00(origin,nruns,doPCA)

#------------------------------------------------------------------------------------
# [3.] create the figures
#------------------------------------------------------------------------------------
  plot_Figure_1a(origin)
  plot_Figure_1b(origin)
  plot_Figure_2(origin)
  plot_figure_3a(origin)
  out <- plot_Figure_3b(origin)
      require(likert)
      pdf(file=file.path(origin,"figures","figure_3",paste0("figure_3b_R",".pdf")))
      par(mfrow=c(1,1),mar=c(8,8,2,2))
      try(plot(out$dfs,group.order = out$target_order) + 
            ggtitle("")+ 
            #        theme(axis.text.y = element_text(size=17,colour = c(color_to_traits(out$target_order1)[17:1])))+
            theme(axis.text.y = element_text(size=15))+
            ylab("% of trait variance explained by climate and/or soil")+
            scale_fill_manual(values=c(climate_col, soil_col, "lightgray"),drop=TRUE))
      dev.off()
  plot_Figure_4(origin)
   
#------------------------------------------------------------------------------------
# [4.] create the tables
#------------------------------------------------------------------------------------
t1 <- table_1(origin)
  
#------------------------------------------------------------------------------------
# [5.] get nunmbers mentioned in the manuscript
#------------------------------------------------------------------------------------
  list.files(file.path(origin,"data", "_results","HierarchicalPartitioning","_RidgeRegression_results"))
  load(file.path(origin,"data", "_results","HierarchicalPartitioning","_RidgeRegression_results",paste0("HP_",nruns,"nruns_doPCA.RData")))
  load(file.path(origin,"data", "_results","HierarchicalPartitioning","_RidgeRegression_results",paste0("R2_",nruns,"nruns_doPCA.RData")))
  # for loading other model results, see "002b_Hierarchical_Partitioning.R" for paths.

  res=r2_l$soilAclimate$r2_soilAclimate
  print("Overall, size traits are better explained by the climate and soil dataset")
  print(paste0("Size trait maximum r2: ",round(max(colMeans(res[,put_into_traitGroup(colnames(res))=="Size"])),digits = 2)))
  print(paste0("Size trait mean r2: ",round(mean(colMeans(res[,put_into_traitGroup(colnames(res))=="Size"])),digits = 2)))
  print(paste0("Economics trait maximum r2: ",round(max(colMeans(res[,put_into_traitGroup(colnames(res))=="Eco"])),digits = 2)))
  print(paste0("Economics trait mean r2: ",round(mean(colMeans(res[,put_into_traitGroup(colnames(res))=="Eco"])),digits = 2)))

  res_c=hp_l$soilAclimate$indep_climate-hp_l$soilAclimate$joint_climate
  res_s=hp_l$soilAclimate$indep_soil-hp_l$soilAclimate$joint_soil
  print("Overall, size traits are explained by climate variables and economics traits by the climate and soil variables")
  print(paste0("Size trait indep effect CLIMATE (mean): ",round(mean(colMeans(res_c[,put_into_traitGroup(colnames(res_c))=="Size"])),digits = 2)))
  print(paste0("Economics trait indep effect CLIMATE (mean): ",round(mean(colMeans(res_c[,put_into_traitGroup(colnames(res_c))=="Eco"])),digits = 2)))
  print(paste0("Size trait indep effect SOIL (mean): ",round(mean(colMeans(res_s[,put_into_traitGroup(colnames(res_s))=="Size"])),digits = 2)))
  print(paste0("Economics trait indep effect SOIL (mean): ",round(mean(colMeans(res_s[,put_into_traitGroup(colnames(res_s))=="Eco"])),digits = 2)))
  

  res1=colMeans(r2_l$soilAclimate$r2_soilAclimate)
  res2=colMeans(r2_l$soilAclimate$r2_soil)
  res3=colMeans(r2_l$soilAclimate$r2_climate)
  
# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  
  # Thank you for downloading the analyses of this study!
  # With questions and comments, please contact: julia.joswig@geo.uzh.ch or (juliajoswigjj@gmail.com)
  
  