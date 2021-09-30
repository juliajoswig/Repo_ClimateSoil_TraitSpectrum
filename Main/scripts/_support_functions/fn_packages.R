
# check if packages exist:


# glmnet
if(!"Matrix" %in% rownames(installed.packages())){install.packages("Matrix")}
if(!"glmnet" %in% rownames(installed.packages())){install.packages("glmnet")}
if(!"FactoMineR" %in% rownames(installed.packages())){install.packages("FactoMineR")}
if(!"randomForest" %in% rownames(installed.packages())){install.packages("randomForest")}
if(!"pls" %in% rownames(installed.packages())){install.packages("pls")}
if(!"gbm" %in% rownames(installed.packages())){install.packages("gbm")}
if(!"vegan" %in% rownames(installed.packages())){install.packages("vegan")}
if(!"ks" %in% rownames(installed.packages())){install.packages("ks")}
if(!"stats" %in% rownames(installed.packages())){install.packages("stats")}
if(!"calibrate" %in% rownames(installed.packages())){install.packages("calibrate")}
if(!"foreach" %in% rownames(installed.packages())){install.packages("foreach")}
if(!"doParallel" %in% rownames(installed.packages())){install.packages("doParallel")}# model building

if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr")}#fig2
if(!"segmented" %in% rownames(installed.packages())){install.packages("segmented")} #fig2
if(!"mnormt" %in% rownames(installed.packages())){install.packages("mnormt", repo = 'https://mac.R-project.org')} #fig3b
if(!"likert" %in% rownames(installed.packages())){install.packages("likert")} #fig3b #https://rpubs.com/m_dev/likert_summary
if(!"sjPlot" %in% rownames(installed.packages())){install.packages("sjPlot")} #fig3b
if(!"sjmisc" %in% rownames(installed.packages())){install.packages("sjmisc")} #fig3b
if(!"wordcloud" %in% rownames(installed.packages())){install.packages("wordcloud")} #fig4
if(!"tm" %in% rownames(installed.packages())){install.packages("tm")} #fig4
if(!"mvtnorm" %in% rownames(installed.packages())){install.packages("mvtnorm")} #fig4
if(!"xtable" %in% rownames(installed.packages())){install.packages("xtable")} #tabs
if(!"textplot" %in% rownames(installed.packages())){install.packages("textplot")} #tabs
if(!"ncdf4" %in% rownames(installed.packages())){install.packages("ncdf4")} 
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"SpatialEpi" %in% rownames(installed.packages())){install.packages("SpatialEpi")}
if(!"spdep" %in% rownames(installed.packages())){install.packages("spdep")}
if(!"rgdal" %in% rownames(installed.packages())){install.packages("rgdal")}


require(glmnet)


#if(!"Matrix" %in% rownames(installed.packages())){install.packages("Matrix")}
print("all packages installed.")
