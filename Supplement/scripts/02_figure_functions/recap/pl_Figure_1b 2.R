
plot_Figure_1b <- function(origin){
  
  # packages
  library(vegan)
  library(ks) 
  library(stats) 
  library(calibrate)

  #-----------------
  # load data
  #-----------------
  load(file.path(origin,"data","helper_files","fig_1b","PCA.RData"))   # load PCA
  load(file.path(origin,"data","helper_files","fig_1b","est.RData"))
  load(file.path(origin,"data","helper_files","fig_1b","H.RData"))
  
  # set contour probabilities for drawing contour levels
  cl <- contourLevels(est, prob=c(0.8, 0.5, 0.05, 0.01), approx=TRUE)
  
  de_fit.m <- data.frame(matrix(data=NA,nrow=17,ncol=2))
  de_fit.m[,1] <- pca_FMr$var$coord[,1]
  de_fit.m[,2] <- pca_FMr$var$coord[,2]
  
  rownames(de_fit.m) <- rownames(pca_FMr$var$coord)
  colnames(de_fit.m) <- c("PC1","PC2")
  
  # arrows
  act.place.x <- vector(mode="numeric",length=nrow(de_fit.m))
  act.place.y <- vector(mode="numeric",length=nrow(de_fit.m))
  de_fit.m <- cbind(de_fit.m,act.place.x,act.place.y)


  #----------------------------------------------------------------------
  # plot PCA
  #----------------------------------------------------------------------
  
{
  if(output_term==""){pdf(file=file.path(origin, "figures","figure_1","figure_1b_PCA.pdf"),height=14,width=16)}

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
       labels = paste0("(",round(pca_FMr$eig$`percentage of variance`[1],digits = 0)," %)"),tick = FALSE,
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
       labels = paste0("(",round(pca_FMr$eig$`percentage of variance`[2],digits = 0)," %)"),tick = FALSE,
       cex.axis=3,line = 5.2)

  plot(est,abs.cont=cl[1], labels="0.2",labcex=1, add=TRUE, lwd=1)
  plot(est,abs.cont=cl[2], labels="0.5",labcex=1, add=TRUE, lwd=1)
  plot(est,abs.cont=cl[3], labels="0.95",labcex=1, add=TRUE, lwd=1)
  plot(est,abs.cont=cl[4], labels="0.99",labcex=1, add=TRUE, lwd=1)
  
  abline(h=0, lty=3, lwd=0.5)
  abline(v=0, lty=3, lwd=0.5)
  
  ##########################################################################################
  # add arrows and trait names
 
  i=2
  for(i in 1:nrow(de_fit.m)){
    x <- c(0,de_fit.m[i,1]*8)
    y <- c(0,de_fit.m[i,2]*8)
    ## draw arrows from point to point :
    s <- seq(length(x)-1)  # one shorter than data
    arrows(x[s], y[s], x[s+1], y[s+1],angle = 25, col = color_to_traits(rownames(de_fit.m)[i]),lwd=2.2)
    adjx=1.1;adjy=1.1
       if(sum(rownames(de_fit.m)[i]%in%c("DispULen"))==1){adjy=1.2;adjy=1.3}
       if(sum(rownames(de_fit.m)[i]%in%c("SeLen"))==1){adjx=1.27;adjy=1.17}
       if(sum(rownames(de_fit.m)[i]%in%c("SeedMass"))==1){adjx=1.16;adjy=0.84}
       if(sum(rownames(de_fit.m)[i]%in%c("PlantHeight"))==1){adjx=1.13;adjy=1.0}
      if(sum(rownames(de_fit.m)[i]%in%c("SenbU"))==1){adjx=1;adjy=1.3}
      if(sum(rownames(de_fit.m)[i]%in%c("SLA"))==1){adjx=1.05;adjy=1.05}
      if(sum(rownames(de_fit.m)[i]%in%c("LeN"))==1){adjx=1.05;adjy=1.05}
      if(sum(rownames(de_fit.m)[i]%in%c("LeP"))==1){adjx=1.05;adjy=1.05}
      if(sum(rownames(de_fit.m)[i]%in%c("Led15N"))==1){adjx=1.1;adjy=1.3}
       if(sum(rownames(de_fit.m)[i]%in%c("VesLen"))==1){adjx=1.1;adjy=1.25}
       if(sum(rownames(de_fit.m)[i]%in%c("LeNP"))==1){adjx=1.3}
    if(sum(rownames(de_fit.m)[i]%in%c("VesLen","Led15N","SenbU"))==1){
#    legend(x[2]*adjx, y[2]*adjy,"", title=Rename_Vars(rownames(de_fit.m)[i])[3], title.col = color_to_traits(rownames(de_fit.m)[i]),
#           box.lwd=.0001, box.col=add.alpha("white",.9),xjust=.5,yjust=.5,col="white",
#           text.col = color_to_traits(rownames(de_fit.m)[i]), fill=add.alpha("white",.9))
      text((x[2]*adjx), (y[2]*adjy)-.01, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text((x[2]*adjx), (y[2]*adjy)-.02, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text((x[2]*adjx), (y[2]*adjy)+.01, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text((x[2]*adjx), (y[2]*adjy)+.02, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text((x[2]*adjx)-.01, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text((x[2]*adjx)-.02, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text((x[2]*adjx)+.01, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text((x[2]*adjx)+.02, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=add.alpha("white",.9))
      text(x[2]*adjx, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],cex=1.9,col=color_to_traits(rownames(de_fit.m)[i])) 
      }else{
    text(x[2]*adjx, y[2]*adjy, labels = Rename_Vars(rownames(de_fit.m)[i])[3],
         cex=1.9,col=color_to_traits(rownames(de_fit.m)[i])) }
  }

dev.off()
}

  
}
