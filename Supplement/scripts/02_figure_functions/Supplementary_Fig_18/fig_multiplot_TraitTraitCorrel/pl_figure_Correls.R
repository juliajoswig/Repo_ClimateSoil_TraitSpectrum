pl_multiplot_S4x <- function(){
  
  # get correl
  load(file = file.path(origin,"data","master_matrix","X1.RData"))
  trait1 <- as.data.frame(TRY_Env1[,Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_gf",replacement = ""))[,2]=="trait"])
  names(trait1) <- gsub(colnames(trait1),pattern = "_gf",replacement = "")
  trait_cor <- cor(log(trait1),method = "pearson")
  #trait_abs <- abs(trait_cor)
  
  par(mfrow=c(3,7))
  
  my_palette <- colorRampPalette(c("white","#edf8fb","#b3cde3","#8c96c6","#88419d"))(n = 499)# Plum
  my_palette <- colorRampPalette(c("white","#fef0d9","#fdcc8a","#fc8d59","#d7301f"))(n = 499)# Terra Cotta
  my_palette <- colorRampPalette(c("white","#f7fcb9","#addd8e","#31a354"))(n = 499)# summer green
  
  col_breaks = c(seq(0,0.1,length=100),  # for red
                 seq(.11,.2,length=100),
                 seq(.21,+.35,length=100),
                 seq(0.36,0.5,length=100), # for yellow
                 seq(0.51,1,length=100))     # for blue
  
  dat_cor_p <- round(abs(trait_cor),digits=2)
#-----------------------------------------------------
  
  
  if(!file.exists(file.path(origin,"figures","Supplement_Fig_18_33"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_18_33"))
  }
  if(!file.exists(file.path(origin,"figures","Supplement_Fig_18_33","Correls"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_18_33","Correls"))
  }
  
  trait_now=1
  for(trait_now in 1:17){
  
    plot_data <-  c(NA,dat_cor_p[trait_now,put_into_traitGroup(colnames(trait1))=="Size"],
                    NA,dat_cor_p[trait_now,put_into_traitGroup(colnames(trait1))=="Eco"],
                    NA,dat_cor_p[trait_now,put_into_traitGroup(colnames(trait1))=="Other"])
  
  pdf(file = file.path(origin,"figures","Supplement_Fig_18_33","Correls",paste0(colnames(trait)[trait_now],"_Correl.pdf")),width=13,height=5)
  par(mfrow=c(3,8),mar=c(0,0,0,0))
  n=0
  i=4
  for(i in 1:length(plot_data)){
    val_now = plot_data[i]
    col_now <- my_palette[col_breaks>=val_now][1]
    plot(c(-1,1),c(-.5,.5),xaxt="n",yaxt="n",frame=F,xlab="",ylab="",col="white")
    if(names(trait1)[trait_now]==names(plot_data)[i]){
      points(0,0,pch=15,cex=16,col="#31a354")  
      text(0,0,labels = round(val_now,digits = 2),pch=16,cex=2,col="black")  
      text(0,0.4,labels=Rename_Vars(names(plot_data)[i])[,3],cex=1.5,col=color_to_traits(names(plot_data)[i]))
      }else{
    points(0,0,pch=16,cex=16,col=col_now)  
    text(0,0.4,labels=Rename_Vars(names(plot_data)[i])[,3],cex=1.5,col=color_to_traits(names(plot_data)[i]))
    if(is.na(val_now)){
      n=n+1
      if(n==1){text(0,0,labels = "Size",pch=16,cex=4,col=color_to_traits("LeArea"))}
      if(n==2){text(0,0,labels = "Eco",pch=16,cex=4,col=color_to_traits("LeN"))}
      if(n==3){text(0,0,labels = "Other",pch=16,cex=4,col=color_to_traits("VesLen"))}
    }else{
    text(0,0,labels = round(val_now,digits = 2),pch=16,cex=2,col="black")  
    }
    }
  }
  dev.off() 
  
   }
  
  

}
