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
  #load(file.path(origin,"data","master_matrix",paste0("TRY_Env2_",output_term,"_2021.RData")))
  
  if(!file.exists(file.path(origin,"figures","figure_S_Multipanel"))){
    dir.create(file.path(origin,"figures","figure_S_Multipanel"))
  }
  if(!file.exists(file.path(origin,"figures","figure_S_Multipanel","Trait_Environment"))){
    dir.create(file.path(origin,"figures","figure_S_Multipanel","Trait_Environment"))
  }
  
  
  t=1
  pdf(file=file.path(origin,"figures","figure_S_Multipanel","Trait_Environment",paste0("Trait_environment",output_term,".pdf")),width=7,height=21)
  par(mar=c(5,10,0,0))
  t=1
  for(t in 1:ncol(TRY_Env$trait)){
  #  for(t in 1:3){
      
      cors <- cor(cbind(TRY_Env$trait[,t],TRY_Env$climate,TRY_Env$soil),method = "pearson")[-1,1]
  
      #Some sample data
      x <-  c(1,0,as.vector(unlist(out$r2_vars[t,])))
      dat <- data.frame(x = x,y = x + 1)
      rbPal <- colorRampPalette(c("white","orange",'red'))
      dat$Col <- rbPal(15)[as.numeric(cut(dat$y,breaks = 15))]
      colz <- dat$Col[-c(1,2)]
      #plot(dat$x,dat$y,pch = 20,col = dat$Col)
      
      
      names(cors) <- Rename_Vars(c(colnames(TRY_Env$climate),colnames(TRY_Env$soil)))[,3]
      barplot(cors,horiz = TRUE,las=2,col=colz,xlim=c(-.7,.7),xlab="Pearson correlation coefficient")

  }
  dev.off()

  }
