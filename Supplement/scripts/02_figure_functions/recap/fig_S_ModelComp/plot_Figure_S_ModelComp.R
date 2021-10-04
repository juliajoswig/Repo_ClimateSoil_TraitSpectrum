Figure_S3 <- function(origin,Appr_type_now){
  
  
  source(file.path(origin,"scripts","01_analysis_functions","HierarchicalPartitioning","fn_hp_N2_wrapper.R"))
  # define data set to be loaded
  sel_now="soilAclimate"
  folds=10
  ncomp_PCA=20
  doPCA=TRUE
  output_term=""#"_sel20_"# "sel20SoilExtend_"
  
  source(file.path(origin,"scripts","01_analysis_functions","RidgeRegression","fn_easyload_RR.R"))
  out_RR <- load_analysis(origin,nruns=50,climOsoil,output_term,doPCA)  
  source(file.path(origin,"scripts","01_analysis_functions","PLS","fn_easyload_PLS.R"))
  out_PLS <- load_analysis(origin,nruns=10,climOsoil,output_term,doPCA)  
  out_PLS_nopca <- load_analysis(origin,nruns=10,climOsoil,output_term,doPCA=FALSE) 
  source(file.path(origin,"scripts","01_analysis_functions","RandomForest","fn_easyload_RandomForest.R"))
  out_RF <- load_analysis(origin,nruns=10,climOsoil,output_term,do_PCA)  
  
  
  hp_RR <- hierarchical_partitioning(origin,output_term,climOsoil,out_now=out_RR)
  hp_RF <-hierarchical_partitioning(origin,output_term,climOsoil,out_now=out_RF)
  hp_PLS <- hierarchical_partitioning(origin,output_term,climOsoil,out_now=out_PLS)
  hp_PLS_nopca <- hierarchical_partitioning(origin,output_term,climOsoil,out_now=out_PLS_nopca)
  
  if(!dir.exists(file.path(origin,"figures","figure_S_ModelComp"))){
    dir.create(file.path(origin,"figures","figure_S_ModelComp"))}
  
  
  pdf(file=file.path(origin,"figures","figure_S_ModelComp","Figure_S_ModelComp.pdf"),width=10,height=20)
    par(mfrow=c(4,1),mar=c(7,6,3,3))
    colz=c("#a6611a","#dfc27d","#80cdc1","#018571")
    ix=order(put_into_traitGroup(colnames(out_RR$r2_soilAclimate)))
    barplot(rbind(colMeans(out_RR$r2_soilAclimate)[ix],
                  colMeans(out_RF$r2_soilAclimate)[ix],
                  colMeans(out_PLS$r2_soilAclimate)[ix],
                  colMeans(out_PLS_nopca$r2_soilAclimate)[ix]),
            ylim=c(0,1),las=2,ylab="Variance explained",
            main="Variance Explained from soil with climate",cex.axis = 0.9,cex.names = 1.2,
            beside = T,col=colz) #col=c("#e66101","#5e3c99","#b2abd2","#fdb863"))
    
    legend("topright", c("Ridge Regression","Random Forest","PLS","PLS without PCA"), 
           col = colz,  pch = 15, bg = "white")
    
    barplot(rbind(colMeans(hp_RR$indep_soil-hp_RR$joint_soil)[ix],
                  colMeans(hp_RF$indep_soil-hp_RF$joint_soil)[ix],
                  colMeans(hp_PLS$indep_soil-hp_PLS$joint_soil)[ix],
                  colMeans(hp_PLS_nopca$indep_soil-hp_PLS_nopca$joint_soil)[ix]),las=2,ylim=c(0,.3),ylab="Variance explained",
            main="Soil independent effect ",cex.axis = 0.9,cex.names = 1.2,
            beside = T,col=colz) #col=c("#e66101","#5e3c99","#b2abd2","#fdb863"))
    legend("topright", c("Ridge Regression","Random Forest","PLS","PLS without PCA"), 
           col = colz,  pch = 15, bg = "white")
    
    barplot(rbind(colMeans(hp_RR$indep_climate-hp_RR$joint_climate)[ix],
                  colMeans(hp_RF$indep_climate-hp_RF$joint_climate)[ix],
                  colMeans(hp_PLS$indep_climate-hp_PLS$joint_climate)[ix],
                  colMeans(hp_PLS_nopca$indep_climate-hp_PLS_nopca$joint_climate)[ix]),las=2,ylim=c(0,.3),ylab="Variance explained",
            main="Climate independent effect ",cex.axis = 0.9,cex.names = 1.2,
            beside = T,col=colz) #col=c("#e66101","#5e3c99","#b2abd2","#fdb863"))
    legend("topright", c("Ridge Regression","Random Forest","PLS","PLS without PCA"), 
           col = colz,  pch = 15, bg = "white")
    
    barplot(rbind(colMeans(2*hp_RR$joint_climate)[ix],
                  colMeans(2*hp_RF$joint_climate)[ix],
                  colMeans(2*hp_PLS$joint_climate)[ix],
                  colMeans(2*hp_PLS_nopca$joint_climate)[ix]),las=2,ylim=c(0,1),ylab="Variance explained",
            main="Total joint effect",cex.axis = 0.9,cex.names = 1.2,
            beside = T,col=colz) #col=c("#e66101","#5e3c99","#b2abd2","#fdb863"))
    legend("topright", c("Ridge Regression","Random Forest","PLS","PLS without PCA"), 
           col = colz,  pch = 15, bg = "white")
    
   dev.off()
  
  print("Fig S3 (model comparison) plotted.")
  
}