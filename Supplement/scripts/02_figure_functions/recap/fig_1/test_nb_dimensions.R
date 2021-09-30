#origin = "/Users/jjoswig/Documents/_docs/03_projects/2020/002_Dichotomy/2019_Revision/20191004_Revision/R/Submission/01_Complete_for_submission"
#origin="/Net/Groups/BGI/work_1/2018_Dichotomy/FINAL/Submission/01_Complete_for_submission"

test_nb_dimensions_PCA <- function(){
  install.packages("ade4")
  require(ade4)
  output_term=""
  
  # ---------------------------------------------------------------------------------------
  # load data species scale A1
  # ---------------------------------------------------------------------------------------
  {
  list.files(file.path(origin,"data","master_matrix"))
  if(output_term==""){load(file = file.path(origin,"data","master_matrix","TRY_Env1_20191111.RData"))}
  if(output_term=="woody"){load(file = file.path(origin,"data","master_matrix","_aggregated_agg1","NA_mnNbr","TRY_Env1woody_20200214.RData"))
    TRY_Env1=TRY_Env1_w}
  if(output_term=="non_woody"){load(file = file.path(origin,"data","master_matrix","_aggregated_agg1","NA_mnNbr","TRY_Env1non_woody_20200214.RData"))
    TRY_Env1=TRY_Env1_nw}
  if(output_term=="other"){load(file = file.path(origin,"data","master_matrix","_aggregated_agg1","NA_mnNbr","TRY_Env1_other_20200214.RData"))
    TRY_Env1=TRY_Env1_o}
  TRY_Env_o<- TRY_Env1
  
  # cut to relevant Ecoregions:
  load(file.path(origin,"data","helper_files","Ecoregions_selected","Ecoregion_Agg2.RData"))
  TRY_Env1=TRY_Env1[TRY_Env1$Group.2%in%Ecoregion_Agg2$ECO_NAME,]
  
  trait <- as.data.frame(TRY_Env1[,Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_gf",replacement = ""))[,2]=="trait"])
  names(trait) <- gsub(colnames(trait),pattern = "_gf",replacement = "")
  climate <- as.data.frame(TRY_Env1[,Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_gf",replacement = ""))[,2]=="climate"])
  names(climate)
  soil <- as.data.frame(TRY_Env1[,Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_gf",replacement = ""))[,2]=="soil"])
  #### add 2020-08-27
  names(soil)
  soil <- soil[,-grep(names(soil),pattern = "_sl2")]
  soil <- soil[,-grep(names(soil),pattern = "_sl3")]
  soil <- soil[,-grep(names(soil),pattern = "_sl4")]
  soil <- soil[,-grep(names(soil),pattern = "_sl5")]
  soil <- soil[,-grep(names(soil),pattern = "_sl6")]
  soil <- soil[,-grep(names(soil),pattern = "_sl7")]
  soil <- soil[,-grep(names(soil),pattern = "_sd2")]
  soil <- soil[,-grep(names(soil),pattern = "_sd3")]
  soil <- soil[,-grep(names(soil),pattern = "_sd4")]
  soil <- soil[,-grep(names(soil),pattern = "_sd5")]
  soil <- soil[,-grep(names(soil),pattern = "_sd6")]
  ### end addtiion 2020-08-27
  names(soil)
  }
  
  if(1!=1){  
    dat <- log(trait)
    
    pca_trait <- dudi.pca(dat,scannf=FALSE)
    sign_dim_traits <- testdim(pca_trait)
    test1
    save(sign_dim_traits,file=file.path(origin,"dim_PCA.RData"))
    print(sign_dim_traits$nb)
    sign_dim_traits$nb
    sign_dim_traits$nb.cor
  }
  
  if(1!=1){  
    pca_trait <- dudi.pca(dat,scannf=FALSE)
  for(i in 1:ncol(dat)){
    x <- c(0,de_fit.m[i,1]*8)
    y <- c(0,de_fit.m[i,2]*8)
    ## draw arrows from point to point :
    s <- seq(length(x)-1)  # one shorter than data
    arrows(x[s], y[s], x[s+1], y[s+1],angle = 25, col = color_to_traits(colnames(dat)[i]),lwd=2.2)
    adjx=1.1;adjy=1.1
    text(x[2]*adjx, y[2]*adjy, labels = rownames(de_fit.m)[i],
         cex=1.9,col=color_to_traits(colnames(dat)[i]))
  }
    plot(pca_trait)
    text(pca_trait)
    pca_trait
    
  }
  
  origin = "/Users/jjoswig/Documents/_docs/03_projects/2020/002_Dichotomy/2019_Revision/20191004_Revision/R/Submission/01_Complete_for_submission"
  
  #------------------------------------------------------------------------------------
  # load data ER scale A2
  #------------------------------------------------------------------------------------
  {
    if(output_term==""|output_term=="PCA"){load(file = file.path(origin,"data","master_matrix","TRY_Env2.RData"))}
  if(output_term=="woody"){load(file = file.path(origin,"data","master_matrix","TRY_Env2_woody.RData"))}
  if(output_term=="non_woody"){load(file = file.path(origin,"data","master_matrix","TRY_Env2_non_woody.RData"))}
  
  info=TRY_Env$info
  trait=TRY_Env$trait
  if(output_term=="PCA"){trait <-TRY_Env$trait_pca}
  soil=TRY_Env$soil
  #### add 2020-08-27
  names(soil)
  soil <- soil[,-grep(names(soil),pattern = "_sl2")]
  soil <- soil[,-grep(names(soil),pattern = "_sl3")]
  soil <- soil[,-grep(names(soil),pattern = "_sl4")]
  soil <- soil[,-grep(names(soil),pattern = "_sl5")]
  soil <- soil[,-grep(names(soil),pattern = "_sl6")]
  soil <- soil[,-grep(names(soil),pattern = "_sl7")]
  soil <- soil[,-grep(names(soil),pattern = "_sd2")]
  soil <- soil[,-grep(names(soil),pattern = "_sd3")]
  soil <- soil[,-grep(names(soil),pattern = "_sd4")]
  soil <- soil[,-grep(names(soil),pattern = "_sd5")]
  soil <- soil[,-grep(names(soil),pattern = "_sd6")]
  names(soil)
  ### end addtiion 2020-08-27
  climate=TRY_Env$climate
  noise = TRY_Env$noise
  climate_energy = TRY_Env$climate_energy
  climate_water = TRY_Env$climate_water
  soil_physics=TRY_Env$soil_physics
  soil_chemistry=TRY_Env$soil_chemistry

    
  }
  
  
  if(1!=1){  
    dat <- log(trait)
    
    pca_trait <- dudi.pca(dat,scannf=FALSE)
    test1 <- testdim(pca1)
    test1
    save(test1,file=file.path(origin,"dim_PCA_trait_ERscale.RData"))
    test1$nb
    test1$nb.cor
    print(test1$nb.cor)
  }
  
  
  if(1!=1){  
    pca1 <- dudi.pca(climate,scannf=FALSE)
    dim_climate <- testdim(pca1)
    dim_climate
    save(dim_climate,file=file.path(origin,"dim_PCA_climate_ERscale.RData"))
    dim_climate$nb
    dim_climate$nb.cor
    print( dim_climate$nb.cor)
  }
  
  
  if(1!=1){  
    pca1 <- dudi.pca(soil,scannf=FALSE)
    dim_soil <- testdim(pca1)
    dim_soil
    save(dim_soil,file=file.path(origin,"dim_PCA_soil_ERscale.RData"))
    dim_soil$nb
    dim_soil$nb.cor
    print(dim_soil$nb.cor)
  }
  }
}

  #------------------------------------------------------------------------------------
  