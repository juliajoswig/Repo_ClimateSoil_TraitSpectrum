# Masterscript accompanying the manuscript Joswig et al. 2020
# Julia Joswig
# ------------

# ---------------------------------------------------------------------------------------
# define the origin path
# ---------------------------------------------------------------------------------------
setwd("/Net/Groups")
orig_loctem ="/Volumes/BGI" # local
#orig_loctem =  "BGI"# terminal 

# origin = # please add your local path here & comment the ones below.
"/Volumes/bgi/work_1/2018_Dichotomy/FINAL/Submission/01_Complete_for_submission"
origin = file.path(orig_loctem,"work_1/2018_Dichotomy/FINAL/Submission/01_Complete_for_submission")
#origin = file.path("/Users/jjoswig/Documents/_docs/03_projects/2020/002_Dichotomy/2019_Revision/20191004_Revision/R")
origin_Agg0data =file.path(origin,"data/master_matrix")
NA_now="NA_mnNbr" #"NA_mnNbr"# or "NA_stays_NA"
list.files(file.path(orig_loctem,"work_1/2018_Dichotomy/FINAL/Submission/01_Complete_for_submission"))
list.files(file.path(origin,"scripts/_master"))

# origin = # please add your local path here & comment the ones below.

# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
# Errors: load table_1.R no working 
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))# check, this does not exist.

  #------------------------------------------------------------------------------------
  # default: output_term = ""
  # uncomment to activate woody or non-woody data set.
  #------------------------------------------------------------------------------------
  output_terms=c("woody","non_woody","obs","grid","NOsel","PCA","climLat","soilLat")#,"biome","coord","woody","non-woody", ""
  output_terms=c("woody","non_woody","")#,"biome","coord","woody","non-woody", ""
  output_term = "obs" # defines with which subset the analysis will be done  (total)
  output_terms = c("climLat","soilLat") # defines with which subset the analysis will be done  (latitude versus soil or climate)
  output_term = "" # defines with which subset the analysis will be done  (latitude versus soil or climate)
  #output_term = "grid" # defines with which subset the analysis will be done  (total)
  #------------------------------------------------------------------------------------
  # load data for visualization purposes
  #------------------------------------------------------------------------------------
  list.files(file.path(origin,"data","master_matrix"))
  load(file = file.path(origin,"data","master_matrix",paste0("TRY_Env2_",output_term,"_2021.RData")))
  head(TRY_Env$trait)
  #------------------------------------------------------------------------------------
  # [1.] model building analyses 
  # input folder: data/master_matrix
  # output folder: data/_results (output files for nruns=50 already available)
  #------------------------------------------------------------------------------------
  for(output_term in output_terms){# loop through all analyses
    print(output_term)  
  
    nruns = 50 # number of model building repetitions for ridge regression final version = 50, for quick version nruns=2
    doPLS = FALSE# define if PLS analysis to be done
    doPCA = FALSE # define if PCA prior to PLS is to be done analysis to be done
    doRR  = FALSE # define if ridge regression analysis to be done
    doRF  = FALSE# define if random forest analysis to be done
    doLM  = TRUE
  
    model_analyses(origin, output_term,  nruns, doPLS, doPCA, doRR, doRF,doLM)
  }

  # rename res_ and res_woody and res_non_woody files.
  #------------------------------------------------------------------------------------
  # [2.] hierarchical partitioning analyses
  # input folder:  data/_results/(model type)/(selected variables)/nruns/
  # output folder: data/_results/(model type)/_(model type)_results
  #------------------------------------------------------------------------------------
  output_terms=c("obs","NOsel","grid","woody","non-woody","PCA")#,"coord","biome","climLat","soilLat"
  output_terms = "PCA" 
  output_term = "" # defines with which subset the analysis will be done  (total)
  for(output_term in output_terms){# loop through all analyses
   print(output_term)  
    
  nruns=50 # number of model building repetitions 
  # for ridge regression final version = 50, for quick version nruns=2
  # for PLS nruns = 10
  # for RF nruns = 10
  doPLS = FALSE# define if hierarchichal paritioning f or PLS analysis to be done
  doPCA = FALSE # define if hierarchichal paritioning for PCA prior to RR or PLS is to be done analysis to be done
  doRR  = TRUE # define if hierarchichal paritioning for ridge regression analysis to be done, woody, non-woody and "" need to be done when run
  doRF  = FALSE# define if hierarchichal paritioning for random forest analysis to be done

  total_hierarchical_partitioning_analyses(origin,output_term,Appr_type_now,nruns,doPLS,doPCA,doRR,doRF) # ONLY 1 can be TRUE
  }
  # Find results of model building under HP_results$r2_l
  # Find results of hierarchical partitioning under HP_results$hp_l
  
  #------------------------------------------------------------------------------------
  # [3.] create the figures
  #------------------------------------------------------------------------------------
  # Manuscript
  plot_Figure_1a(origin)
  plot_Figure_1b(origin)
  plot_Figure_2(origin)
  plot_figure_3a(origin)
  out <- plot_Figure_3b(origin,nruns=2,doPCA=TRUE)
    require(likert)
    pdf(file=file.path(origin,"figures","figure_3",paste0("figure_3b_R",".pdf")))
    par(mfrow=c(1,1),mar=c(8,8,2,2))
    try(plot(out$dfs,group.order = out$target_order1) + 
          ggtitle("")+ 
          ylab("% of trait variance explained by climate and/or soil")+
          scale_fill_manual(values=c(climate_col, soil_col, "lightgray"),drop=TRUE))
    dev.off()
  plot_Figure_4(origin,trait,soil,climate,info)
  
  # Appendix
  # woody non-woody
  # PCA:
  plot_Figure_S_PCA_loadings(origin)
#  plot_Figure_S_WoNa(origin,output_term="woody")
#  plot_Figure_S_WoNa(origin,output_term="non_woody")
  plot_Figure_S_WoNb(origin,output_term="woody")
  plot_Figure_S_WoNb(origin,output_term="non_woody")
  plot_Figure_S_WoNc(origin,output_term="woody")
  plot_Figure_S_WoNc(origin,output_term="non_woody")
  # Pattern robustness
  plot_Figure_S_TraitsEnv_lm(origin,climOsoil,output_term)
  plot_Figure_S_PCA_HP(origin,doPCA)
  # Model robustness
  out <- plot_Figure_S_Noise(origin,nruns=50,doPCA,climOsoil="noiseAclimate")
      require(likert)
      pdf(file=file.path(origin,"figures","figure_S_Noise",paste0("figure_S_Noise_noiseAclimate.pdf")))
      par(mfrow=c(1,1),mar=c(8,8,2,2))
      try(plot(out$dfs,group.order = out$target_order1) + 
            ggtitle(title)+ 
            ylab("% of trait variance explained by climate and/or noise")+
            scale_fill_manual(values=c(climate_col, soil_col, "lightgray"),drop=TRUE))
      dev.off()
  out <- plot_Figure_S_Noise(origin,nruns=50,doPCA,climOsoil="soilAnoise")
      pdf(file=file.path(origin,"figures","figure_S_Noise",paste0("figure_S_Noise_soilAnoise.pdf")))
      par(mfrow=c(1,1),mar=c(8,8,2,2))
      try(plot(out$dfs,group.order = out$target_order1) + 
            ggtitle(title)+ 
            ylab("% of trait variance explained by the soil and/or noise")+
            scale_fill_manual(values=c(climate_col, soil_col, "lightgray"),drop=TRUE))
      dev.off()
  pl_CoefVar(origin)    
  out <- plot_Figure_S_Obs(origin,nruns=50,doPCA,climOsoil="soilAnoise")
        pdf(file=file.path(origin,"figures","figure_S_Obs",paste0("figure_S_Obs_doPCA_noSelmin50ER.pdf")))
        par(mfrow=c(1,1),mar=c(8,8,2,2))
        try(plot(out$dfs,group.order = out$target_order1) + 
              ggtitle(title)+ 
              ylab("% of trait variance explained by the soil and/or noise")+
              scale_fill_manual(values=c(climate_col, soil_col, "lightgray"),drop=TRUE))
        dev.off()
  pl_Figure_S_DipBiome(origin)

  #------------------------------------------------------------------------------------
  # [4.] create the tables
  #------------------------------------------------------------------------------------
  # Manuscript
  t1 <- table_1(origin)
  # 
  tS_TraitInfo <- tab_S_TraitInfo(origin,TRY_Env$info)
  tab_S_ER(origin,trait,info)
  
  #------------------------------------------------------------------------------------
  # Additions:
  nruns=50
  output_term=""
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("HP_",nruns,"nruns_", output_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("R2_",nruns,"nruns_", output_term,".RData")))
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
  

# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  
  # Thank you for downloading the analyses of this study!
  # With questions and comments, please contact: jjoswig@bgc-jena.mpg.de or m
  
  