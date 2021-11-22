
# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
# origin = # please add your local path here & comment the ones below.
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Supplement" # please add your local path here 
list.files(file.path(origin,"scripts/_master"))

# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
# packages
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))


if(!file.exists(file.path(origin,"figures","Supplement_Fig_18"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_18"))}
if(!file.exists(file.path(origin,"figures","Supplement_Fig_18","Trait_Environment"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_18","Trait_Environment"))}

  type_analysis="lm"
  climOsoil="soilAclimate"
  doPCA=FALSE
  
  output_term=""
  #output_term="PCA"
  
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  list.files(file.path(origin, "data", "_results","lm",climOsoil, paste0(nruns,"Reps")))
  load(file=file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                      paste0("Res_",output_term,pca_term,".RData")))
  
  library("gplots")
  colnames(out$r2_vars) <- Rename_Vars(colnames(out$r2_vars))[,3]
  list.files(file.path(origin,"data","master_matrix"))
  load(file = file.path(origin,"data","master_matrix",paste0("X2_",output_term,".RData")))
  trait=TRY_Env$trait
  load(file = file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),paste0("Res_",output_term,pca_term,".RData")))
  
#  if(output_term=="PCA"){
#    trait_pca <- PCA(log(trait))
#    TRY_Env$trait <- trait$ind$coord[,1:3]
#    trait=TRY_Env$trait
#    }  

  colnames(out$r2_vars) <- Rename_Vars(colnames(out$r2_vars))[,3]
  target_order=c("LeArea","PlantHeight" ,"SeedMass","SeLen","LeFMass","ConduitDens",
                 "DispULen","SSD","SLA" ,"LeC","LeN","LeP", "LeNArea","LeNP","Led15N","SenbU","VesLen")
  if(output_term=="PCA"){target_order=c("Dim.1","Dim.2","Dim.3","Dim.4","Dim.5")}
  colnames(TRY_Env$trait)
  
  t=1
  pdf(file=file.path(origin,"figures","Supplement_Fig_18","Trait_Environment",paste0("Trait_environment",output_term,"_lm.pdf")),width=7,
      height=25)
  par(mar=c(5,10,2,1))
  t=3
  #for(t1 in 1:ncol(TRY_Env$trait)){
  #t=which(colnames(TRY_Env$trait)%in%target_order[t1])
  if(output_term==""){ts=1:17}
  if(output_term=="PCA"){ts=1:3}
  for(t in ts){
      
      cors <- cor(cbind(TRY_Env$trait[,t],TRY_Env$climate,TRY_Env$soil),method = "pearson")[-1,1]
  
      #Some sample data
      x <-  c(1,0,cors)
      dat <- data.frame(x = x,y = x + 1)
      rbPal <- colorRampPalette(c("blue","white",'red'))
      dat$Col <- rbPal(15)[as.numeric(cut(dat$y,breaks = 15))]
      colz <- dat$Col[-c(1,2)]
      #plot(dat$x,dat$y,pch = 20,col = dat$Col)
      
      
      barplot(out$r2_vars[t,],
              horiz = TRUE,
              las=2,col=colz,
              xlim=c(0,.8),
              xlab="Variance explained by lm",
              main=colnames(TRY_Env$trait)[t],
              cex.main=2,cex.lab = 2)
      
      
  }
  dev.off()

  
