# run this on the cluster when possible
origin
output_term="woody"
#output_term="non_woody"

  # packages
  library(vegan)
  library(ks) 
  library(stats) 
  library(calibrate)


  #----------------------------------------------------------------------
  # load data
  #----------------------------------------------------------------------
  trait_names=c("LeArea","SSD","SLA","LeC","LeN","LeP","PlantHeight", "SeedMass","SeLen","LeNArea","LeNP",
                "Led15N","SenbU","LeFMass","ConduitDens" ,"DispULen","VesLen"   )
  # read data
  list.files(file.path(origin,"data","master_matrix"))
  load(file.path(origin,"data","master_matrix","X1.RData"))
  X1_tot <- as.data.frame(read.csv(file.path(origin,"data","master_matrix",paste0("X1_",output_term,".csv"))))
  X1_gf <- X1_tot[,grep(colnames(X1_tot),pattern = "_gf")]
  colnames(X1_gf) <- gsub(colnames(X1_gf),pattern = "_gf",replacement = "")
  head(X1_gf)
  X1_gf <- X1_gf[,Rename_Vars(colnames(X1_gf))[,2]=="trait"]
  X1_log <- as.data.frame(log(X1_gf))
  
  #----------------------------------------------------------------------
  # do PCA
  #----------------------------------------------------------------------
  require(FactoMineR)
  pca_traits <- PCA(X1_log,ncp = 2,graph = FALSE)
  
  #----------------------------------------------------------------------
  # calculate the window size 
  #----------------------------------------------------------------------
  sc = pca_traits$ind$coord

  if(file.exists(file.path(origin,"data","helper_files","fig_S8",output_term,"H.RData"))){
    load(file.path(origin,"data","helper_files","fig_S8",output_term,"H.RData"))
    load(file.path(origin,"data","helper_files","fig_S8",output_term,"est.RData"))
  }else{
    H <- Hpi(x=sc) # optimal bandwidth estimation
    save(H,file=file.path(origin,"data","helper_files","fig_S8",output_term,"H.RData"))
    est <- kde(x=sc, H=H, compute.cont=TRUE) # kernel density estimation
    save(est,file=file.path(origin,"data","helper_files","fig_S8",output_term,"est.RData"))
    print("kernel estimation done!")
  }
  
  # set contour probabilities for drawing contour levels
  cl <- contourLevels(est, prob=c(0.8, 0.5, 0.05, 0.01), approx=TRUE)
  
  de_fit.m <- data.frame(matrix(data=NA,nrow=17,ncol=2))
  de_fit.m[,1] <- pca_traits$var$coord[,1]
  de_fit.m[,2] <- pca_traits$var$coord[,2]
  pca_traits$eig[3,]
  
  rownames(de_fit.m) <- rownames(pca_traits$var$coord)
  colnames(de_fit.m) <- c("PC1","PC2")
  
  # arrows
  act.place.x <- vector(mode="numeric",length=nrow(de_fit.m))
  act.place.y <- vector(mode="numeric",length=nrow(de_fit.m))
  de_fit.m <- cbind(de_fit.m,act.place.x,act.place.y)


  #----------------------------------------------------------------------
  # plot PCA
  #----------------------------------------------------------------------
  
  if(!dir.exists(file.path(origin, "figures","Supplement_Fig_8"))){dir.create(file.path(origin, "figures","Supplement_Fig_8"))}
  
{
  pdf(file=file.path(origin, "figures","Supplement_Fig_8",paste0("Supplement_fig_8",output_term,"_PCA.pdf")),height=14,width=16)

  par(mfrow=c(1,1),mar=c(9,9,2,2),bg="white")
  plot(est, cont=seq(1,100,by=2), display="filled.contour2", add=FALSE, cex.axis=3,cex.lab=3,frame=F,
       ylab="",xaxt="n",yaxt="n",
       xlab="",
        xlim=c(-10,10),
        ylim=c(-10,10)
  )
  ## add the tick
  axis(1, at = seq(from=-10,to=10,by = 5), label = rep("",5) , tck = -0.02,cex.lab=3)
  ## add the labels
  axis(1, at = seq(from=-10,to=10,by = 5), line = 2, lwd = 0, cex.axis = 0.9,cex.axis=3)
  ## add the label
  axis(side = 1,at=-.2,
       labels = expression('PC'[1]),tick = FALSE,
       cex.axis=3,line = 6)
  axis(side = 1,at=2,
       labels = paste0("(",round(pca_traits$eig[1,2],digits = 0)," %)"),tick = FALSE,
       cex.axis=3,line = 5.6)

  ## add the tick
  axis(2, at = seq(from=-10,to=10,by = 5), label = rep("",5) , tck = -0.02,cex.lab=3)
  ## add the labels
  axis(2, at = seq(from=-10,to=10,by = 5), line = 2, lwd = 0, cex.axis = 0.9,cex.axis=3)
  ## add the label
  axis(side = 2,at=-.8,
       labels = expression('PC'[2]),tick = FALSE,
       cex.axis=3,line = 4.9)
  axis(side = 2,at=2,
       labels = paste0("(",round(pca_traits$eig[2,2],digits = 0)," %)"),tick = FALSE,
       cex.axis=3,line = 5.2)

  plot(est,abs.cont=cl[1], labels="0.2",labcex=2, add=TRUE, lwd=1)
  plot(est,abs.cont=cl[2], labels="0.5",labcex=2, add=TRUE, lwd=1)
  plot(est,abs.cont=cl[3], labels="0.95",labcex=2, add=TRUE, lwd=1)
  plot(est,abs.cont=cl[4], labels="0.99",labcex=2, add=TRUE, lwd=1)
  
  abline(h=0, lty=3, lwd=0.5)
  abline(v=0, lty=3, lwd=0.5)
  
  ##########################################################################################
  # add arrows and trait names
  text_size=2.5
  i=2
  for(i in 1:nrow(de_fit.m)){
    x <- c(0,de_fit.m[i,1]*8)
    y <- c(0,de_fit.m[i,2]*8)
    ## draw arrows from point to point :
    s <- seq(length(x)-1)  # one shorter than data
    arrows(x[s], y[s], x[s+1], y[s+1],angle = 25, col = color_to_traits(rownames(de_fit.m)[i]),lwd=2.2)
    adjx=1.1;adjy=1.1
    # for visual purposes adjust the position of the trait name
    if(sum(rownames(de_fit.m)[i]%in%c("ConduitDens"))==1){adjx=1.1;adjy=1.2}
    if(sum(rownames(de_fit.m)[i]%in%c("DispULen"))==1){adjy=1.2;adjy=1.4}
    if(sum(rownames(de_fit.m)[i]%in%c("SeLen"))==1){adjx=1.20;adjy=1.17}
       if(sum(rownames(de_fit.m)[i]%in%c("SeedMass"))==1){adjx=1.18;adjy=0.74}
       if(sum(rownames(de_fit.m)[i]%in%c("PlantHeight"))==1){adjx=1.15;adjy=.6}
      if(sum(rownames(de_fit.m)[i]%in%c("SenbU"))==1){adjx=1;adjy=1.3}
    if(sum(rownames(de_fit.m)[i]%in%c("SLA"))==1){adjx=1.05;adjy=1.07}
    if(sum(rownames(de_fit.m)[i]%in%c("SSD"))==1){adjx=1.4;adjy=1.1}
    if(sum(rownames(de_fit.m)[i]%in%c("LeN"))==1){adjx=1.05;adjy=1.07}
    if(sum(rownames(de_fit.m)[i]%in%c("LeNArea"))==1){adjx=1.1;adjy=1.27}
    if(sum(rownames(de_fit.m)[i]%in%c("LeP"))==1){adjx=1.05;adjy=1.07}
      if(sum(rownames(de_fit.m)[i]%in%c("Led15N"))==1){adjx=1.1;adjy=1.3}
       if(sum(rownames(de_fit.m)[i]%in%c("VesLen"))==1){adjx=1.1;adjy=1.25}
       if(sum(rownames(de_fit.m)[i]%in%c("LeNP"))==1){adjx=1.5}
    if(sum(rownames(de_fit.m)[i]%in%c("VesLen","Led15N","SenbU"))==1){
      text((x[2]*adjx), (y[2]*adjy)-.01, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text((x[2]*adjx), (y[2]*adjy)-.02, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text((x[2]*adjx), (y[2]*adjy)+.01, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text((x[2]*adjx), (y[2]*adjy)+.02, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text((x[2]*adjx)-.01, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text((x[2]*adjx)-.02, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text((x[2]*adjx)+.01, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text((x[2]*adjx)+.02, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=add.alpha("white",.9))
      text(x[2]*adjx, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=text_size,col=color_to_traits(rownames(de_fit.m)[i])) 
      }else{
    text(x[2]*adjx, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],
         cex=text_size,col=color_to_traits(rownames(de_fit.m)[i])) }
  }

dev.off()
}

  

