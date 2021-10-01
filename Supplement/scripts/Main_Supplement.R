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
list.files(file.path(orig_loctem,"work_1/2018_Dichotomy/FINAL/Submission/00_Prepare_for_submission"))
origin = file.path(orig_loctem,"work_1/2018_Dichotomy/FINAL/NEE/Supplement")
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/Repo_ClimateSoil_TraitSpectrum/Supplement" 

#origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Supplement"
list.files(file.path(origin,"scripts/_master"))

# origin = # please add your local path here & comment the ones below.

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
output_term = "non_woody" # defines with which subset the analysis will be done  (latitude versus soil or climate)
list.files(file.path(origin,"data","master_matrix"))
load(file = file.path(origin,"data","master_matrix",paste0("X2_",output_term,"_NEE.RData")))
head(TRY_Env$trait)
dim(TRY_Env$trait)

#------------------------------------------------------------------------------------
# 3. model building analyses 
# input folder: data/master_matrix
# output folder: data/_results
#------------------------------------------------------------------------------------
for(output_term in output_terms){# loop through all analyses which takes a couple of hours
  print(output_term)  
  
  nruns_RR = 50 # number of model building repetitions for ridge regression final version = 50, for quick version nruns=2
  nruns_LM = 50 # number of model building repetitions for linear models, final version = 50, for quick version nruns=2
  nruns_RF = 2 # number of model building repetitions for random forest, 2 
  nruns_PLS = 10 # number of model building repetitions for linear models, final version = 10, for quick version nruns=2
  
  doPCA = FALSE # define if PCA prior to RR/PLS is to be done analysis to be done (never RF)
  doRR  = FALSE # define if ridge regression analysis to be done
  doPLS = FALSE # define if PLS analysis to be done
  doRF  = FALSE # define if random forest analysis to be done
  doLM  = TRUE# define if linear model analysis to be done

  doPCA = TRUE # define if PCA prior to RR/PLS is to be done analysis to be done (never RF)
  doRR  = TRUE # define if ridge regression analysis to be done
  doPLS = FALSE # define if PLS analysis to be done
  doRF  = FALSE # define if random forest analysis to be done
  doLM  = FALSE # define if linear model analysis to be done

  model_analyses(origin, output_term, doPCA,
                 doRR,    doLM,    doRF,    doPLS,
                 nruns_RR,nruns_LM,nruns_RF,nruns_PLS)
}


#------------------------------------------------------------------------------------
# 2. hierarchical partitioning analyses
# input folder:  data/_results/(model type)/(selected variables)/nruns/
# output folder: data/_results/(model type)/_(model type)_results
#------------------------------------------------------------------------------------
output_term = "non_woody" # defines with which subset the analysis will be done  (total)
for(output_term in output_terms){# loop through all analyses
  print(output_term)  
  
  
  doPLS = FALSE# define if hierarchical partioning f or PLS analysis to be done
  doPCA = TRUE # define if hierarchical partioning for PCA prior to RR or PLS is to be done analysis to be done
  doRR  = TRUE # define if hierarchical partioning for ridge regression analysis to be done, woody, non-woody and "" need to be done when run
  doRF  = FALSE# define if hierarchical partioning for random forest analysis to be done
  
  if(output_term!=""){
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
# Supplement Fig. 3: file.path(origin,"scripts","02_figure_functions","Supplementary_Fig_3","fig_Bias.R")

if(!file.exists(file.path(origin,"figures","Supplement_Fig_3"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_3"))}

if(!file.exists(file.path(origin,"figures","Supplement_Fig_17"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_17"))}
source(file.path(origin,"scripts","02_figure_functions","Supplemenary Fig 17","fig_ModelComp.R"))#install.packages "likert"


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

