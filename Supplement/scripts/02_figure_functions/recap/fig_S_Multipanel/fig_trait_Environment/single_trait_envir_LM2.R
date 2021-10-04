plot_Figure_S_TraitsEnv_lm <- function(origin,climOsoil,nruns,output_term,doPCA){
  
  type_analysis="lm"
  climOsoil="soilAclimate"
  doPCA=FALSE
  
  output_term=""
  output_term="PCA"
  
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  list.files(file.path(origin, "data", "_results","lm",climOsoil, paste0(nruns,"Reps")))
  load(file=file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                      paste0("Res_",output_term,pca_term,".RData")))
  
  library("gplots")
  colnames(out$r2_vars) <- Rename_Vars(colnames(out$r2_vars))[,3]
  load(file = file.path(origin,"data","master_matrix",paste0("TRY_Env2_",output_term,"_2021.RData")))
  load(file = file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),paste0("Res_",output_term,pca_term,".RData")))
  trait=TRY_Env$trait
  trait <- PCA(log(trait),ncp = 3)
  TRY_Env$trait <- trait$ind$coord
  #load(file.path(origin,"data","master_matrix",paste0("TRY_Env2_",output_term,"_2021.RData")))
  
  if(!file.exists(file.path(origin,"figures","figure_S_Multipanel"))){
    dir.create(file.path(origin,"figures","figure_S_Multipanel"))
  }
  if(!file.exists(file.path(origin,"figures","figure_S_Multipanel","Trait_Environment"))){
    dir.create(file.path(origin,"figures","figure_S_Multipanel","Trait_Environment"))
  }
  
  
  colnames(out$r2_vars) <- Rename_Vars(colnames(out$r2_vars))[,3]
  target_order=c("LeArea","PlantHeight" ,"SeedMass","SeLen","LeFMass","ConduitDens",
                 "DispULen","SSD","SLA" ,"LeC","LeN","LeP", "LeNArea","LeNP","Led15N","SenbU","VesLen")
  if(output_term=="PCA"){target_order=c("Dim.1","Dim.2","Dim.3")}
  colnames(TRY_Env$trait)
  
  t=1
  pdf(file=file.path(origin,"figures","figure_S_Multipanel","Trait_Environment",paste0("Trait_environment",output_term,"_lm.pdf")),width=7,
      height=25)
  par(mar=c(5,10,2,1))
  t=1
  #for(t1 in 1:ncol(TRY_Env$trait)){
  #t=which(colnames(TRY_Env$trait)%in%target_order[t1])
    for(t in 1:3){
      
      cors <- cor(cbind(TRY_Env$trait[,t],TRY_Env$climate,TRY_Env$soil),method = "pearson")[-1,1]
  
      #Some sample data
      x <-  c(1,0,cors)
      dat <- data.frame(x = x,y = x + 1)
      rbPal <- colorRampPalette(c("blue","white",'red'))
      dat$Col <- rbPal(15)[as.numeric(cut(dat$y,breaks = 15))]
      colz <- dat$Col[-c(1,2)]
      #plot(dat$x,dat$y,pch = 20,col = dat$Col)
      
      
      barplot(out$r2_vars[t,],horiz = TRUE,las=2,col=colz,xlim=c(0,.8),xlab="Variance explained by lm",main=colnames(TRY_Env$trait)[t],
              cex.main=2,cex.lab = 2)
      
      
  }
  dev.off()

  }
