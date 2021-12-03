# Masterscript for the supplementary analyses of the study 
# "Climatic and soil factors explain the two-dimensional spectrum of global plant trait variation"
# written by Julia Joswig
# contributions are marked where applicable
# ------------


# Content
# 01. define the origin path
# 02 load data for visualization purposes
# 3. model building analyses 
# 2. hierarchical partitioning analyses


# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
setwd("/Net/Groups")
orig_loctem ="/Volumes/BGI" # local
orig_loctem =  "BGI"#"/Volumes/BGI" 
output_term = "" 
origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/Repo_ClimateSoil_TraitSpectrum/Supplement" 
# origin = # please add your local path here & comment the ones below.

list.files(file.path(origin,"scripts/_master"))


# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))# check, this does not exist.

#------------------------------------------------------------------------------------
# default: output_term = ""
# uncomment to activate woody or non-woody data set.
#------------------------------------------------------------------------------------
output_terms=c(
  "bias1","bias2","bias3", # traits randomized across the X0 (individual plants) level, then aggregated to ecoregions
  "", # original input data
  "woody","non_woody", # input data includes only woody or non-woody species
  "obs", # input data not with gap-filled, but observed data
  "grid", # input data not aggregated to ecoregions, but to grids
  "NOsel", # input data/ Ecoregions without selection criteria
  "PCA", # input traits as their first PCs from PCA
  "climLat","soilLat" # climate or soil variables replaced with latitudinal information
  )# 

#------------------------------------------------------------------------------------
# 02 load data for visualization purposes
#------------------------------------------------------------------------------------
output_term = "obs" # defines with which subset the analysis will be done  (latitude versus soil or climate)
list.files(file.path(origin,"data","master_matrix"))
load(file = file.path(origin,"data","master_matrix",paste0("X2_",output_term,"_NEE.RData")))
head(TRY_Env$trait)
dim(TRY_Env$trait)

#------------------------------------------------------------------------------------
# 3. Produce all model building analyses for all supplement figures
# input folder: data/master_matrix
# output folder: data/_results
#------------------------------------------------------------------------------------
run_again=FALSE # set = TRUE, if you want to run the models again.
nruns_RR = 50 # number of model building repetitions for ridge regression final version = 50, for quick version nruns=2
nruns_LM = 50 # number of model building repetitions for linear models, final version = 50, for quick version nruns=2
nruns_RF = 2 # number of model building repetitions for random forest, 2 
nruns_PLS = 10 # number of model building repetitions for linear models, final version = 10, for quick version nruns=2

if(run_again=TRUE){
for(output_term in output_terms){# loop through all analyses which takes a couple of hours
  print(output_term)  

  # here: please adjust according to your needs
  doPCA = FALSE # define if PCA prior to RR/PLS is to be done analysis to be done (never RF)
  doRR  = FALSE # define if ridge regression analysis to be done
  doPLS = FALSE # define if PLS analysis to be done
  doRF  = FALSE # define if random forest analysis to be done
  doLM  = TRUE# define if linear model analysis to be done
  
  
  model_analyses(origin, output_term, doPCA,
                 doRR,    doLM,    doRF,    doPLS,
                 nruns_RR,nruns_LM,nruns_RF,nruns_PLS)
}
}

#------------------------------------------------------------------------------------
# 2. hierarchical partitioning analyses
# input folder:  data/_results/(model type)/(selected variables)/nruns/
# output folder: data/_results/(model type)/_(model type)_results
#------------------------------------------------------------------------------------
# output_term = "" # defines with which subset the analysis will be done  (total)
for(output_term in output_terms){# loop through all analyses
  print(output_term)  
  
  doPLS = FALSE# define if hierarchical partioning f or PLS analysis to be done
  doPCA = TRUE # define if hierarchical partioning for PCA prior to RR or PLS is to be done analysis to be done
  doRR  = TRUE # define if hierarchical partioning for ridge regression analysis to be done, woody, non-woody and "" need to be done when run
  doRF  = FALSE# define if hierarchical partioning for random forest analysis to be done
  
  if(output_term!=""){ # only Ridge Regression utilized.
  total_hierarchical_partitioning_analyses(origin,output_term,
                                           doPLS,doPCA,doRR,doRF,
                                           nruns_RR,nruns_LM,nruns_RF,nruns_PLS) 
  }

  if(output_term==""){
  for(mo_now in 1:4){
    print(paste(c("RR","RF","PLS_PCA","PLS_noPCA")[mo_now]))
    doPLS = FALSE# define if hierarchical paritioning f or PLS analysis to be done
    doPCA = TRUE # define if hierarchical paritioning for PCA prior to RR or PLS is to be done analysis to be done
    doRR  = TRUE # define if hierarchical paritioning for ridge regression analysis to be done, woody, non-woody and "" need to be done when run
    doRF  = FALSE# define if hierarchical paritioning for random forest analysis to be done
    
    if(mo_now==2){
      doPLS = FALSE# define if hierarchical paritioning f or PLS analysis to be done
      doPCA = FALSE # define if hierarchical paritioning for PCA prior to RR or PLS is to be done analysis to be done
      doRR  = FALSE # define if hierarchical paritioning for ridge regression analysis to be done, woody, non-woody and "" need to be done when run
      doRF  = TRUE# define if hierarchical paritioning for random forest analysis to be done
    }    
    if(mo_now==3){
      doPLS = TRUE# define if hierarchical paritioning f or PLS analysis to be done
      doPCA = TRUE # define if hierarchical paritioning for PCA prior to RR or PLS is to be done analysis to be done
      doRR  = FALSE # define if hierarchical paritioning for ridge regression analysis to be done, woody, non-woody and "" need to be done when run
      doRF  = FALSE# define if hierarchical paritioning for random forest analysis to be done
    }    
    if(mo_now==4){
      doPLS = TRUE# define if hierarchical paritioning f or PLS analysis to be done
      doPCA = FALSE # define if hierarchical paritioning for PCA prior to RR or PLS is to be done analysis to be done
      doRR  = FALSE # define if hierarchical paritioning for ridge regression analysis to be done, woody, non-woody and "" need to be done when run
      doRF  = FALSE# define if hierarchical paritioning for random forest analysis to be done
    }    
    
  total_hierarchical_partitioning_analyses(origin,output_term,
                                           doPLS,doPCA,doRR,doRF,
                                           nruns_RR,nruns_LM,nruns_RF,nruns_PLS) 
  }
  }
}
# Find results of model building under HP_results$r2_l
# Find results of hierarchical partitioning under HP_results$hp_l

#------------------------------------------------------------------------------------
# [3.] create the figures
#------------------------------------------------------------------------------------
# Supplement Fig. 1: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_1","fig_Noise.R")
# Supplement Fig. 2: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_2","fig_Bias.R")
# with output_term="bias1" AND "bias2" AND "bias3"
# Supplement Fig. 3: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_3","pl_Figure_S_noSelComp.R") 
# Supplement Fig. 4: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_4","fig_") # MISSING
# Supplement Fig. 5: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_5","pl_Figure_PCA_loadings_VarExpl.R")
# Supplement Fig. 6: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_6","pl_Figure_PCA_loadings_VarExpl.R")
# Supplement Fig. 7: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_7","pl_Figure_S_WoNb.R")
# Supplement Fig. 8a: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_8","Sup_Fig_8a.R")
# Supplement Fig. 8b: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_8","Sup_Fig_8b.R")
# Supplement Fig. 9: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_9","Sup_Fig_9.R")
# Supplement Fig. 10a: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_10","Fig_TTT_1of2.R")
# Supplement Fig. 10b: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_10","Fig_TTT_2of2.R")
# Supplement Fig. 11: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_11","fig_latClimSoil.R")
# Supplement Fig. 12: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_12","fig_WoN.R")
# Supplement Fig. 13: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_13","pl_Figure_S_TraitsEnv_lm.R")
# Supplement Fig. 14: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_14","pl_figure_S_PCA_HP.R")
# Supplement Fig. 15: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_15","pl_Figure_S_Obs.R")
# Supplement Fig. 16: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_16","pl_Figure_S_TraitsEnv_lmOBSERVED.R")
# Supplement Fig. 17: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_17","fig_ModelComp.R")

# For Supplement Figures 18 to 39
# Template Barplot: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_multipanel_Barplot,"pl_figure_Barplot_TEMPLATE.R")
# Template Riverplot: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_multipanel_Riverplot", "pl_figure_Riverplot_TEMPLATE.R")
# Template Correlation: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_multiplot_TraitTraitCorrel", "pl_figure_Correls_TEMPLATE.R")
# Barplots: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_multipanel_Barplot,"pl_figure_Barplot.R")
# Latitudinal Gradients: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_multipanel_Latitudinal_gradient","fn_figure_LatGradient.R")
# Riverplot: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_multipanel_Riverplot", "pl_figure_Riverplot.R")
# Correlation: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_multiplot_TraitTraitCorrel", "pl_figure_Correls.R")
# Trait-Environment relationships: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18","fig_trait_Environment", "single_trait_envir")
# "fig_traitEnv"
# Function putting the figures together:
# file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18", "Latex_Fig18to39_1of2.tex")
# file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_18", "Latex_Fig18to39_2of2.tex")

# Supplement Fig. 40: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_40","pl_Figure_S_DipBiome.R")
# Supplement Fig. 41: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_41","pl_Figure_S_Dip29to36PrecSand.R")
# Supplement Fig. 42: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_42","pl_Figure_S_CoefVar.R")


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

# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  # Thank you for downloading the analyses of this study!
  # With questions and comments, please contact: jjoswig@bgc-jena.mpg.de or m

