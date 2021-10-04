analysis_pls <- function(climate,soil,trait,
                         origin,climOsoil,folds,nruns,
                         output_term,chunk_list,doPCA,ncomp_PCA){
  
  require("stats")
  require("FactoMineR")
  require("glmnet") 
  require("gbm")
  require("pls")

  #----------------------------------------------------------------
  # PCA
  #---------------------------------------------------------------
  
  if(doPCA){
    pca_clim <- PCA(climate,ncp = ncomp_PCA,graph = FALSE)
    pca_clim=pca_clim$ind$coord
    
    pca_soil <- PCA(soil,ncp = ncomp_PCA,graph = FALSE)
    pca_soil=pca_soil$ind$coord
  }
  
  if(!doPCA){
    pca_clim <- climate
    pca_soil <- soil
}
  
  
  if(climOsoil=="climate"){pred_pca <- pca_clim}
  if(climOsoil=="soil"){pred_pca <- pca_soil}
  if(climOsoil=="soilAclimate"){pred_pca <- cbind(pca_clim,pca_soil)}

  pred <- as.data.frame(pred_pca)

  response <- log(trait)
  resp <- response

  #---------------------------------------------------------------
  # create output files
  nbdf <- matrix(ncol=ncol(resp),nrow=nruns)
  r2 <- matrix(ncol=ncol(resp),nrow=nruns)
  colnames(r2) <- colnames(resp)
  colnames(nbdf) <- colnames(resp)
  fit_list <- list()
  predicted <- list()

  for(tr in 1:ncol(resp)){
      print(paste0("-----------------------------------------"))
      print(colnames(resp)[tr])
      p <- matrix(NA,nrow(resp),ncol=nruns)
      for(n in 1:nruns){
        print(paste0("CV run nb ",n,"of ",nruns))
        NumLVs <- rep(NA,folds)
        for(rep in 1:folds){
          
          ix_bt <- !((1:nrow(pred))%in%chunk_list[[n]][[rep]])
          
          #build model
          data.now <- data.frame(indepdent = pred, response = resp[,tr])
          ncomp = 10
          fold = nrow(data.now[ix_bt,])-1
          PartitionType="random"
          
          plsr.fit <- mvr(response ~ ., ncomp, data = data.now[ix_bt,], 
                          method = "oscorespls", scale = TRUE)
          CV <- crossval(plsr.fit, segments = fold, data = data.CARS.cal, 
                         segment.type = PartitionType)
          NumLV  <- which.min(sqrt(CV$validation$PRESS/nrow(data.now)))
          ## Predicted responses for models with 1, 2, 3 and 4 components
          pred.resp <- predict(plsr.fit, ncomp = 1:10, newdata = data.now[!ix_bt,])
          ## Predicted scores
          # pred.scores <- predict(plsr.fit, comps = 1:10, type = "scores", newdata = data.now[!ix_bt,])[,NumLV]
          p[!ix_bt,n] <- pred.resp[,,NumLV]
          NumLVs[rep] <- NumLV
          #plot(data.now[!ix_bt,1],pred.resp[,,NumLV])
        } 
        #---------------------------------------------------------------
        # write into output
        #---------------------------------------------------------------
        
        #ix <- which(cvfit$lambda%in%cvfit$lambda.min)
        r2[n,tr]<- cor(resp[,tr],p[,n])^2
        nbdf[n,tr] <- mean(NumLVs,na.rm = TRUE)
        
      }
      predicted[[tr]] <- p 
    }
    
    out <- list(r2 = r2,
                nbdf = nbdf, 
                predicted = predicted)

    
    type_analysis="PLS"
    if(!file.exists(file.path(origin, "data", "_results",type_analysis))){
      dir.create(file.path(origin, "data", "_results",type_analysis))}
    
    #climOsoil
    if(!file.exists(file.path(origin, "data", "_results",type_analysis,climOsoil))){
      dir.create(file.path(origin, "data", "_results",type_analysis,climOsoil))}
    
    # nruns
    if(!file.exists(file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps")))){
      dir.create(file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps")))}
    
    # save file:
    if(doPCA){pca_term="PCA"}
    if(!doPCA){pca_term="no_PCA"}
    save(out, file = file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                               paste0("Res_",output_term,pca_term,".RData")))
    
    print("- - - - - - - ")
    print(climOsoil)
    print("- - - - - - - ")
    print(r2)
    
    return(out)
}
