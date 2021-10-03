

  # load NOsel
  output_term="NOsel"
  pca_term="doPCA"
  nruns=2
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_",output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
  mtc<- match(target_order1,colnames(r2_l$soilAclimate$r2_soilAclimate))
  r2_NOsel <- r2_l$soilAclimate$r2_soilAclimate[,mtc]
  hp_NOsel <- hp_l
  
  # load ""
  output_term=""
  nruns=50
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_",output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
  r2_ <- r2_l$soilAclimate$r2_soilAclimate[,mtc]
  hp_ <- hp_l
  
  barplot(colMeans(hp_$soilAclimate$indep_climate)-
            colMeans(hp_NOsel$soilAclimate$indep_climate),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$indep_soil)-
            colMeans(hp_NOsel$soilAclimate$indep_soil),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$joint_soil)*2-
            colMeans(hp_NOsel$soilAclimate$joint_soil)*2,col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  

  # load "grid"
  output_term="grid"
  nruns=2
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("HP_",nruns,"nruns_",output_term,pca_term,".RData")))
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                 paste0("R2_",nruns,"nruns_",output_term,pca_term,".RData")))
  r2_grid <- r2_l$soilAclimate$r2_soilAclimate[,mtc]
  hp_grid <- hp_l
  
  barplot(colMeans(hp_$soilAclimate$indep_climate)-
            colMeans(hp_grid$soilAclimate$indep_climate),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$indep_soil)-
            colMeans(hp_grid$soilAclimate$indep_soil),col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  barplot(colMeans(hp_$soilAclimate$joint_soil)*2-
            colMeans(hp_grid$soilAclimate$joint_soil)*2,col=color_to_traits(colnames(r2_)),ylim=c(-.3,.3),las=2)
  
  

  if(!dir.exists(file.path(origin,"figures","Supplement_Fig_3"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_3"))}
  
  nms <- colnames(r2_)
  colnames(r2_) <- Rename_Vars(colnames(r2_))[,3]
  colnames(r2_NOsel) <- Rename_Vars(colnames(r2_NOsel))[,3]
  
  pdf(file=file.path(origin,"figures","Supplement_Fig_3",paste0("figure_S_SelAggComparison.pdf")),height=3.5,width=5)
  par(mfrow=c(1,1),mar=c(6,5,1,1))
    barplot(colMeans(r2_)-colMeans(r2_NOsel),col=color_to_traits(nms),ylim=c(-.3,.3),las=2,cex.lab=.8,
          ylab="r2 dist (selected, non-selected)")
    abline(h = seq(from=-.4,to = .4,by = .1),lty=2,col="gray")
    barplot(colMeans(r2_)-colMeans(r2_NOsel),col=color_to_traits(nms),ylim=c(-.3,.3),las=2,add=TRUE)
    
    barplot(colMeans(r2_NOsel)-colMeans(r2_grid),col=color_to_traits(nms),las=2,ylim=c(-.3,.3),cex.lab=.8,
            ylab="r2 dist (non-sel. ER, non-sel. grid)")
    abline(h = seq(from=-.4,to = .4,by = .1),lty=2,col="gray")
    barplot(colMeans(r2_NOsel)-colMeans(r2_grid),col=color_to_traits(nms),ylim=c(-.3,.3),las=2,add=TRUE)

  dev.off()
  
  

