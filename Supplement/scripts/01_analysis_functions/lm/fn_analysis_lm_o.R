analysis_lm <- function(climate,soil,trait,
                         origin,climOsoil,folds,nruns,
                         output_term,chunk_list,doPCA,ncomp_PCA){
  
  require(FactoMineR)
  #----------------------------------------------------------------
  # PCA
  #---------------------------------------------------------------
  
  if(doPCA){
    print("doing PCA")
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
  if(climOsoil=="soilAclimate"&!doPCA){
  r2 <- matrix(ncol=ncol(pred),nrow=ncol(resp))
  colnames(r2) <- colnames(pred)
  rownames(r2) <- colnames(resp)
  r2_tr <- matrix(NA,ncol=ncol(pred),nrow=nruns)
  colnames(r2_tr) <- colnames(pred)
  predicted <- list()
  pred_l <- list()
  for(tr in 1:ncol(resp)){
    print(paste0("-----------------------------------------"))
    print(colnames(resp)[tr])
    p <- matrix(NA,nrow(resp),ncol=nruns)
    
    vr=1
    for(vr in 1:ncol(pred)){
      pred_now=pred[,vr]
      
    for(n in 1:nruns){
      print(paste0("CV run nb ",n,"of ",nruns))
      
      rep=1 
      for(rep in 1:folds){
        
        ix_bt <- !((1:length(pred_now))%in%chunk_list[[n]][[rep]])
        
        #build model
        data.now <- data.frame(indepdent = pred_now, response = resp[,tr])
        fold = nrow(data.now[ix_bt,])-1
        
        lm_out <- lm(response ~ .,  data = data.now[ix_bt,])
        ## Predicted responses for model
        pred_resp <- predict(lm_out, newdata = data.now[!ix_bt,])
        ## Predicted scores
        # pred.scores <- predict(plsr.fit, comps = 1:10, type = "scores", newdata = data.now[!ix_bt,])[,NumLV]
        p[!ix_bt,n] <- pred_resp
      } 
      #---------------------------------------------------------------
      # write into output
      #---------------------------------------------------------------
      r2_tr[n,vr]<- cor(resp[,tr],p[,n])^2
    }
    }
      r2[tr,] <- colMeans(r2_tr)
  }
  
      r2_vars = r2
      r2_vars_single = r2_tr
  }

  
  #---------------------------------------------------------------
  # create output files
  r2 <- matrix(ncol=ncol(resp),nrow=nruns)
  colnames(r2) <- colnames(resp)
  predicted <- list()
  pred_l <- list()
  tr=1 
  for(tr in 1:ncol(resp)){
    print(paste0("-----------------------------------------"))
    print(colnames(resp)[tr])
    p <- matrix(NA,nrow(resp),ncol=nruns)
    
    for(n in 1:nruns){
      print(paste0("CV run nb ",n,"of ",nruns))
      
      fo=1 
      for(fo in 1:folds){
        
        ix_bt <- !((1:nrow(pred))%in%chunk_list[[n]][[fo]])
        
        #build model
        data.now <- data.frame(indepdent = pred, response = resp[,tr])
        fold = nrow(data.now[ix_bt,])-1
        
        lm_out <- lm(response ~ .,  data = data.now[ix_bt,])
        ## Predicted responses for model
        pred.resp <- predict(lm_out, newdata = data.now[!ix_bt,])
        ## Predicted scores
        # pred.scores <- predict(plsr.fit, comps = 1:10, type = "scores", newdata = data.now[!ix_bt,])[,NumLV]
        p[!ix_bt,n] <- pred.resp
      } 
      
      #---------------------------------------------------------------
      # write into output
      #---------------------------------------------------------------
      r2[n,tr]<- cor(resp[,tr],p[,n])^2
      predicted[[n]] <- p 
    }
    pred_l[[tr]] <- predicted
  }
  
  if(climOsoil=="soilAclimate"&!doPCA){
    out <- list(r2 = r2,
               pred_l = pred_l,
               r2_vars = r2_vars,
               r2_vars_single =r2_vars_single)
  }else{    
    out <- list(r2 = r2,
                pred_l = pred_l)
  }
  
  
    type_analysis="lm"
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
