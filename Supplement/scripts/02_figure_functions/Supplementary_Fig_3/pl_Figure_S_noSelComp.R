
# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
# origin = # please add your local path here & comment the ones below.
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Supplement" # please add your local path here 
list.files(file.path(origin,"scripts/_master"))

# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))

target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")

# ---------------------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------------------
# load NOsel
output_term="NOsel"
pca_term="doPCA"
nruns=2

# Load output data
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_",output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
# Order according to the "target order"
  mtc<- match(target_order1,colnames(r2_l$soilAclimate$r2_soilAclimate))
  r2_NOsel <- r2_l$soilAclimate$r2_soilAclimate[,mtc]
  hp_NOsel <- hp_l
  
# load data to main analysis output_term=""
  output_term=""
  nruns=50
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_",output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
  r2_ <- r2_l$soilAclimate$r2_soilAclimate[,mtc]
  hp_ <- hp_l
  # check
  barplot(colMeans(hp_$soilAclimate$indep_climate)-
            colMeans(hp_NOsel$soilAclimate$indep_climate),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$indep_soil)-
            colMeans(hp_NOsel$soilAclimate$indep_soil),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$joint_soil)*2-
            colMeans(hp_NOsel$soilAclimate$joint_soil)*2,col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  

# load output for gridded data run "grid"
  output_term="grid"
  nruns=2
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_",output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
  r2_grid <- r2_l$soilAclimate$r2_soilAclimate[,mtc]
  hp_grid <- hp_l
  
  # check
  barplot(colMeans(hp_$soilAclimate$indep_climate)-
            colMeans(hp_grid$soilAclimate$indep_climate),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$indep_soil)-
            colMeans(hp_grid$soilAclimate$indep_soil),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$joint_soil)*2-
            colMeans(hp_grid$soilAclimate$joint_soil)*2,col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  
  
# create figure folder
  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_3"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_3"))}
  
  nms <- colnames(r2_)
  colnames(r2_) <- Rename_Vars(colnames(r2_))[,3]
  colnames(r2_NOsel) <- Rename_Vars(colnames(r2_NOsel))[,3]

# plot.  
  pdf(file=file.path(origin,"figures","Supplement_Fig_3",paste0("figure_S_SelAggComparison.pdf")),height=4,width=5)
  par(mfrow=c(1,1),mar=c(6,5,1,1))
    barplot(colMeans(r2_)-colMeans(r2_NOsel),col=color_to_traits(nms),ylim=c(-.3,.3),las=2,cex.lab=1,
          ylab="R2 dist (sel., non-sel.)")
    abline(h = seq(from=-.4,to = .4,by = .1),lty=2,col="gray")
    barplot(colMeans(r2_)-colMeans(r2_NOsel),col=color_to_traits(nms),ylim=c(-.3,.3),las=2,add=TRUE)
    
    barplot(colMeans(r2_NOsel)-colMeans(r2_grid),col=color_to_traits(nms),las=2,ylim=c(-.3,.3),cex.lab=1,
            ylab="R2 dist (non-sel. ER, non-sel. grid)")
    abline(h = seq(from=-.4,to = .4,by = .1),lty=2,col="gray")
    barplot(colMeans(r2_NOsel)-colMeans(r2_grid),col=color_to_traits(nms),ylim=c(-.3,.3),las=2,add=TRUE)

  dev.off()
  
  

