analysis_RandomForest <- function(climate,soil,trait,origin,
                                  climOsoil,nruns,folds,output_term,chunk_list = chunk_list,ncomp_PCA){
  require("randomForest")
  pred_choice=climOsoil
  #----------------------------------------------------------------
  # PCA
  #---------------------------------------------------------------

  require(FactoMineR)
  #install.packages("glmnet")
  library(glmnet) 
  require(gbm)
  require(MASS)#package with the boston housing dataset
  
  pca_clim <- PCA(climate,ncp = ncomp_PCA,graph = FALSE)
  pca_soil <- PCA(soil,ncp = ncomp_PCA,graph = FALSE)

  if(climOsoil=="climate"){pred_pca <- pca_clim$ind$coord}
  if(climOsoil=="soil"){pred_pca <- pca_soil$ind$coord}
  if(climOsoil=="soilAclimate"){pred_pca <- cbind(pca_clim$ind$coord,pca_soil$ind$coord)}

  colnames(pred_pca) <- paste0("dim_",1:ncol(pred_pca))
  #try(dev.off())

  pred <- as.data.frame(pred_pca)
    
  response <- log(trait)#;colnames(response) <- paste0(colnames(trait),"_log")
  resp <- response#as.matrix(cbind(response,trait))
  
  #---------------------------------------------------------------
  # create output files
  nbdf <- matrix(ncol=ncol(resp),nrow=nruns)
  r2 <- matrix(ncol=ncol(resp),nrow=nruns)
  colnames(r2) <- colnames(resp)
  colnames(nbdf) <- colnames(resp)
  fit_list <- list()
  predicted<- list()
  
  #try(dev.off())
    rep=1
    tr=1
    n=1
    for(tr in 1:ncol(resp)){
      print(paste0("-----------------------------------------"))
      print(colnames(resp)[tr])
      p <- matrix(NA,nrow(resp),ncol=nruns)
      for(n in 1:nruns){
        print(paste0("CV run nb ",n,"of ",nruns))
        
        for(rep in 1:folds){
          
          ix_bt <- !((1:nrow(pred))%in%chunk_list[[n]][[rep]])
          
          #build model
          data_now=as.data.frame(cbind(as.matrix(resp[,tr]),as.matrix(pred)))
          names(data_now)[1] <- "trait"
          RF_model = randomForest(trait ~ . ,data = data_now[ix_bt,], importance=TRUE,
                       proximity=TRUE)
          # Generating a Prediction matrix for each Tree
          preddat <- predict(RF_model,data_now[!ix_bt,],ntree=101, proximity=TRUE, oob.prox=FALSE)
          
          p[!ix_bt,n] <- preddat$predicted
        } 
      
        #---------------------------------------------------------------
        # write into output
        #---------------------------------------------------------------
        
        #ix <- which(cvfit$lambda%in%cvfit$lambda.min)
        r2[n,tr]<- cor(resp[,tr],p[,n])^2
      
        
      }
      predicted[[tr]] <- p 
    }
    
    
    out <- list(r2=r2,predicted=predicted)
   
    type_analysis="RandomForest"
    #climOsoil
    if(!file.exists(file.path(origin, "data", "_results",type_analysis))){
      dir.create(file.path(origin, "data", "_results",type_analysis))}
    #climOsoil
    if(!file.exists(file.path(origin, "data", "_results",type_analysis,climOsoil))){
      dir.create(file.path(origin, "data", "_results",type_analysis,climOsoil))}
    # nruns
    if(!file.exists(file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps")))){
      dir.create(file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps")))}
    
    save(out, file = file.path(origin, "data", "_results","RandomForest",climOsoil, paste0(nruns,"Reps"),
                               paste0("Res_",output_term,".RData")))
    print(r2)

    
    return(out)
}
