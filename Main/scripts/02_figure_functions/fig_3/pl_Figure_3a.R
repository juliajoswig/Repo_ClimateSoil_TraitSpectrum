plot_figure_3a <- function(origin){
  
    doPCA=TRUE
    climOsoil="soilAclimate"     
    output_term=""
    
    if(!dir.exists(file.path(origin,"figures","figure_3"))){
      dir.create(file.path(origin,"figures","figure_3"))}
    
    # ---------------------------------------------------------
    # load data
    # ---------------------------------------------------------
    if(doPCA){pca_term="doPCA"}
    if(!doPCA){pca_term="no_PCA"}
    load(file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results",
                     paste0("HP_",nruns=50,"nruns_",pca_term,".RData")))
    hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]

    X_hp1=hp_now[[1]]
    X_hp2=hp_now[[2]]
    edg=1
  
  nm=c(paste0(substring(text = climOsoil,first = 1,last = regexpr("A", climOsoil)[1]-1)),
       paste0(sub(".*A", "", climOsoil)))
  library(dplyr)
  fch <- toupper(substr(nm[1],start = 1,stop = 1))
  sch <- toupper(substr(nm[2],start = 1,stop = 1))
  nm[1] <- paste0(fch,substr(nm[1],start = 2,stop = nchar(nm[1])))
  nm[2] <- paste0(sch,substr(nm[2],start = 2,stop = nchar(nm[2])))
  
  print(nm)
  trait.names_tmp <- colnames(X_hp1)
  
  #-----------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------------------
  nl = list.files(file.path(origin,"figures","figure_3"))
  nl = nl[grep(x = nl,pattern = climOsoil)]
  shorty=1
  if(length(nl)){shorty=1}
  if(length(nl)>0){shorty=length(nl)+1}
  
  {
    pdf(file = file.path(origin,"figures","figure_3",paste0("figure_S3a_",shorty,".pdf")),width=15,height=15)
    par(mfrow=c(2,2),mar=c(0,0,0,0))
    
    plot(x=c(0,1),y=c(0,1),pch=16,col=c("white"),ylim=c(0,edg),cex=1, #c(min(TWO[!is.na(ONE)])*1.1,max(TWO[!is.na(ONE)])*1.1),
         xlim=c(0,edg),axes=FALSE,ylab="",xlab="",frame=FALSE)
    text(x = .9, y=.5,labels = paste0(nm[2]," Effect (Independent + Joint/2)"),cex=2.5, srt = -90)#nm[2],
    i=1
    t=1
    dat_plot <- rep(NA,6)
    for(t in c(1:17)){
      ONEt <- X_hp1[,t]
      TWOt <- X_hp2[,t]
      dat_plot <- as.data.frame(rbind(dat_plot,
                                      c(mean(ONEt,na.rm = T),quantile(ONEt,0.05,na.rm = T),quantile(ONEt,0.95,na.rm = T),
                                        mean(TWOt,na.rm = T),quantile(TWOt,0.05,na.rm = T),quantile(TWOt,0.95,na.rm = T))))
    }
    
    
    names(dat_plot) <- c("ONE","ONEi","ONEx","TWO","TWOi","TWOx")
    dat_plot <- dat_plot[!is.na(dat_plot[,1]),]
    rownames(dat_plot) =colnames(X_hp1)
    
    plot_overlayNB(edg,axis_cex=1,climate_col=climate_col_o,soil_col=soil_col_o)
    
    i=1
    for(i in c(1:nrow(dat_plot))){
      points(dat_plot$ONE[i],dat_plot$TWO[i],pch=15,col= color_to_traits(rownames(dat_plot)[i]),cex=2)
    }

    
    for(i in c(1:nrow(dat_plot))){
      adx=.035
      ady=0.006
      print(rownames(dat_plot)[i])
      if(output_term==""){
        if(rownames(dat_plot)[i]=="ConduitDens"){adx=.035;ady=.01}
        if(rownames(dat_plot)[i]=="LeN"){adx=-0.03;ady=-.008}
        if(rownames(dat_plot)[i]=="LeFMass"){adx=-0.075;ady=0.085}
      if(rownames(dat_plot)[i]=="SSD"){adx=-0.05;ady=0.09}
      if(rownames(dat_plot)[i]=="SLA"){adx=-0.03;ady=-0.028}
      if(rownames(dat_plot)[i]=="LeC"){adx=0.0;ady=-0.04}
      if(rownames(dat_plot)[i]=="SeedMass"){adx=-0.075;ady=0.08}
      if(rownames(dat_plot)[i]=="PlantHeight"){adx=-0.055;ady=0.055}      
      if(rownames(dat_plot)[i]=="SeLen"){adx=-0.07;ady=0.07}      
      if(rownames(dat_plot)[i]=="LeNP"){adx=0.084;ady=-0.05}
      if(rownames(dat_plot)[i]=="Led15N"){adx=0.085;ady=-0.06}
      if(rownames(dat_plot)[i]=="DispULen"){adx=0.04;ady=0}
      if(rownames(dat_plot)[i]=="LeNArea"){adx=0.11;ady=-.08}
      if(rownames(dat_plot)[i]=="VesLen"){adx=0.04;ady=-.079}
      }
      for(i2 in c(0.001)){
        j2=0
        text((dat_plot$ONE[i]+adx)+j2,(dat_plot$TWO[i]+ady)+i2,labels = Rename_Vars(rownames(dat_plot)[i])[,3],pch=16,
           col= "white",srt=-45,cex=1.8)
          text((dat_plot$ONE[i]+adx)-j2,(dat_plot$TWO[i]+ady)-i2,labels = Rename_Vars(rownames(dat_plot)[i])[,3],pch=16,
               col= "white",srt=-45,cex=1.8)
        }
      for(j2 in c(0.001)){
        i2=0
        text((dat_plot$ONE[i]+adx)+j2,(dat_plot$TWO[i]+ady)+i2,labels = Rename_Vars(rownames(dat_plot)[i])[,3],pch=16,
             col= "white",srt=-45,cex=1.8)
        text((dat_plot$ONE[i]+adx)-j2,(dat_plot$TWO[i]+ady)-i2,labels = Rename_Vars(rownames(dat_plot)[i])[,3],pch=16,
             col= "white",srt=-45,cex=1.8)
      }
      text(dat_plot$ONE[i]+adx,dat_plot$TWO[i]+ady,labels = Rename_Vars(rownames(dat_plot)[i])[,3],pch=16,
           col= color_to_traits(rownames(dat_plot)[i]),srt=-45,cex=1.8)
    }
    

    plot(x=c(0,1),y=c(1,1),pch=16,col=c("white"),ylim=c(0,edg),cex=1,#c(min(TWO[!is.na(ONE)])*1.1,max(TWO[!is.na(ONE)])*1.1),
         xlim=c(0,edg),axes=FALSE,ylab="",xlab="",frame=FALSE)
    
    plot(x=c(0,1),y=c(1,1),pch=16,col=c("white"),ylim=c(0,edg),cex=1,#c(min(TWO[!is.na(ONE)])*1.1,max(TWO[!is.na(ONE)])*1.1),
         xlim=c(0,edg),axes=FALSE,ylab="",xlab="",frame=FALSE)
    text(x = 0.5, y=.9,labels = paste0(nm[1]," Effect (Independent + Joint/2)"),cex=2.5)#, srt = -90)#nm[1],
    
    legend(.1, .8, c("Size trait", "Eco trait", "Other trait"), 
#           col = c("#5884b4", "#cd6787", "#e6cf6c"),
           col=c(size_col,eco_col,other_col),
          text.col = "black", pch = c(16,16,16),
           merge = FALSE ,border = FALSE,bty="n" ,y.intersp = 1.5,pt.cex = 2.5)

    dev.off()
  }

  
    }
#  }
#}
