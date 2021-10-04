
  require(likert)
  #library(sjPlot)
  #library(sjmisc)
#  data(efc)
  nruns=50
  output_term="obs"
  pca_term="doPCA"
  #-------------
  # load data
  #-------------
  list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results")) 
  
    load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                   paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
    hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]
    title <- "" 
  
  # data processing
      m <- matrix(NA,ncol=4,nrow=ncol(hp_now[[3]]))
      m[,1] <- Rename_Vars(colnames(hp_now[[3]]))[,3]
      m[,1] <- colnames(hp_now[[3]])#Rename_Vars()[,3]
      m[,2] <- colMeans(hp_now[[2]] - hp_now[[4]])*100# pure indep effect
      m[,3] <- colMeans(hp_now[[3]])*2*100# joint effect
      m[,4] <- colMeans(hp_now[[1]] - hp_now[[3]])*100# pure indep effect
      m[m<0]  = 0
      
  colnames(m)<- c("Item","Independent_climate","Joint","Independent_soil")
  df <- data.frame(m)
  df <- transform(df,Independent_climate=as.numeric(as.vector(Independent_climate)),
                  Independent_soil=as.numeric(as.vector(Independent_soil)),Joint=as.numeric(as.vector(Joint)))
  
  # rename & order
  ix <- match(target_order1,df$Item)
  df$Item[ix]
  df <- df[ix,]
  df$Item <- Rename_Vars(df$Item)[,3]
  df <- df[!is.na(df$Joint),]
  
  dfs <- likert(summary = df)#,grouping = put_into_traitGroup(colnames(hp_now$r2_soil)))
  str(dfs)
  #The items are not present, but the likert object can still be summarised:
  summary(dfs)
  #Plots
  
  scale_height = knitr::opts_chunk$get('fig.height')*2.5
  scale_width = knitr::opts_chunk$get('fig.width')*.7
  knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)
  theme_update(legend.text = element_text(size = rel(1)))
  
  out <- list()
  out$dfs <- dfs

  
  if(!file.exists(file.path(origin,"figures","Supplement_Fig_15"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_15"))}
  
  
  pdf(file=file.path(origin,"figures","Supplement_Fig_15",paste0("Supplement_Fig_Obs.pdf")))  
  par(mfrow=c(1,1),mar=c(8,8,2,2))
  
  try(plot(out$dfs,group.order = df$Item) + 
        ggtitle(title)+ 
        ylab("% of trait variance explained by noise and/or soil")+
        scale_fill_manual(values=c(climate_col, "lightgray", soil_col),
                          drop=FALSE))
  dev.off()

