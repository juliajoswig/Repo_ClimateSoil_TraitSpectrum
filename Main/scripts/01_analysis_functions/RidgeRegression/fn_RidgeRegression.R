analysis_RidgeRegression <- function(climOsoil,climate,soil,trait,nruns,chunk_list,ncomp_PCA,folds,doPCA){

  #----------------------------------------------------------------
  # prepare data
  #---------------------------------------------------------------

  require(FactoMineR)
  require(glmnet) 
  
  
  if(!doPCA){
    pca_clim <- climate
    pca_soil <- soil
  }    

  if(doPCA){
   pca_clim <- PCA(climate,ncp = ncomp_PCA,graph = FALSE)
   pca_clim = pca_clim$ind$coord
   
   pca_soil <- PCA(soil,ncp = ncomp_PCA,graph = FALSE)
   pca_soil = pca_soil$ind$coord
   }
  
  if(climOsoil=="climate"){pred_pca <- pca_clim}
  if(climOsoil=="soil"){pred_pca <- pca_soil}
  if(climOsoil=="soilAclimate"){pred_pca <- cbind(pca_clim,pca_soil)}
  
  pred <- as.data.frame(pred_pca)
  response <- log(trait) # log transform the traits
  resp <- response
  
  #---------------------------------------------------------------
  # create output files
  #---------------------------------------------------------------
  
  nbdf <- matrix(ncol=ncol(resp),nrow=nruns)
  r2 <- matrix(ncol=ncol(resp),nrow=nruns)
  colnames(r2) <- colnames(resp)
  colnames(nbdf) <- colnames(resp)
  fit_list <- list()
  predicted<- list()
  
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
          
          #internal cross validation to get lambda min for final model.
          cvfit = cv.glmnet(x=as.matrix(pred[ix_bt,]), y=as.matrix(resp[ix_bt,tr]),nfolds = 10, keep = T)
          
          # predict with final model (defined by lambda min)
          p[!ix_bt,n] = predict(cvfit,newx=as.matrix(pred[!ix_bt,]),s="lambda.min")
        } 
        #---------------------------------------------------------------
        # write into output
        #---------------------------------------------------------------
        
        ix <- which(cvfit$lambda%in%cvfit$lambda.min)
        r2[n,tr]<- cor(resp[,tr],p[,n])^2 # calculate r2
        nbdf[n,tr] <- cvfit$glmnet.fit$df[which(cvfit$lambda%in%cvfit$lambda.min)]
        
      }
      predicted[[tr]] <- p 
    }
    
    print(climOsoil)
    print(r2)
    
    out <- list(r2=r2,nbdf=nbdf)
    
    type_analysis="RidgeRegression"
    # put results section
    if(!file.exists(file.path(origin, "data", "_results"))){
      dir.create(file.path(origin, "data", "_results"))}
    #analysis type
    if(!file.exists(file.path(origin, "data", "_results",type_analysis))){
      dir.create(file.path(origin, "data", "_results",type_analysis))}
    #climOsoil
    if(!file.exists(file.path(origin, "data", "_results",type_analysis,climOsoil))){
      dir.create(file.path(origin, "data", "_results",type_analysis,climOsoil))}
    # nruns
    if(!file.exists(file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps")))){
      dir.create(file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps")))}
    
    # save file:
    if(doPCA){pca_term="doPCA"}
    if(!doPCA){pca_term="no_PCA"}
    save(out, file = file.path(origin, "data", "_results",type_analysis,climOsoil, paste0(nruns,"Reps"),
                               paste0("Res_",pca_term,".RData")))

    
    return(out)
}
