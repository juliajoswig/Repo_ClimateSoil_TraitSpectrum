plot_appendix_individual_traits <- function(origin,atmObio,meanOsd,Agg_type,sel_now,nruns){
  

  
  if(file.exists( file.path( origin, "data", "VarSelect_output", paste0(nruns,"Reps"), fold_now,
                                  paste0("VarSelOutput_",Appr_type,climOsoil,".RData")))){   
    
    #load VarSelOutput_
    load(file.path( origin, "data", "VarSelect_output", paste0(nruns,"Reps"), fold_now,
                          paste0("VarSelOutput_",Appr_type,climOsoil,".RData")))
          
    
    metrics <- VarSelOutput$rel_I_metrics
    r2_2 <- VarSelOutput$accuracy
    soil2 <- VarSelOutput$rel_I_soilWeighted
    metrics[,which(colnames(metrics)%in%c("r2_mean"))]
    atm2 <- VarSelOutput$rel_I_atmWeighted
    trait.names_tmp <- gsub(x = VarSelOutput$colnames_resp,pattern = paste0("_",meanOsd),replacement = "")
    
    edg=1
    
    t=1
    for(t in 1:length(trait.names_tmp)){
      
      pdf(file=file.path(origin,"plots","Fig_S4", paste0(trait.names[t],".pdf")), height=15,width=20)

      par(mfrow=c(2,3),mar=c(0,0,0,0))
      
      plot(x=c(0,1),y=c(1,1),pch=16,col=c("white"),ylim=c(0,edg),cex=1,
           xlim=c(0,edg),axes=FALSE,ylab="",xlab="",frame=FALSE)
      text(x = .9, y=.5,labels = "Climate cumulative importance",cex=3.5, srt = -90)
      
      plot_overlayNB(edg,axis_cex=2)
      
      r2_t <- as.numeric(as.vector(unlist(r2_2[[t]][,2])))
      soilt <- as.numeric(as.vector(unlist(soil2[[t]])))
      atmt <- as.numeric(as.vector(unlist(atm2[[t]])))
      
      dat=as.data.frame(cbind(r2_t,soilt,atmt))
      points(dat$soilt,dat$atmt,col= "black",
             srt=-45,cex=1.7,pch=16)
      points(dat$soilt,dat$atmt,col= add.alpha(color_to_traits(trait.names_tmp[t]),.8),
             srt=-45,cex=1.5,pch=16)
      #  text(mean(dat$soilt),mean(dat$atmt),col= "black",labels = trait.names_tmp[t],
      #       srt=-45,cex=1.5,pch=16)
      text(.45,.45,col= "black",labels = trait.names_tmp[t],
           srt=-45,cex=3,pch=16)
      

      load(file=file.path(file.path(origin, "plots","Appendix_IndivTraits",paste0(trait.names[t],".RData"))))
      
      RPE <- out$RPE
      style <- out$style
      
      
      op = par(cex = 1.2,no.readonly = TRUE)
      riverplot(RPE, 
                gravity = 'center', 
                nodewidth = 7, 
                default_style = style,
                plot_area = .95)
      
      plot(x=c(0,1),y=c(1,1),pch=16,col=c("white"),ylim=c(0,edg),cex=1,
           xlim=c(0,edg),axes=FALSE,ylab="",xlab="",frame=FALSE)
      
      plot(x=c(0,1),y=c(1,1),pch=16,col=c("white"),ylim=c(0,edg),cex=1,
           xlim=c(0,edg),axes=FALSE,ylab="",xlab="",frame=FALSE)
      text(x = 0.5, y=.9,labels = "Soil cumulative importance",cex=2)
      
      dev.off()
    }
  
  }
  
}