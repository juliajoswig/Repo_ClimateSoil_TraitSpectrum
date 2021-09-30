#install.packages("likert")
#whichfig="3b"
plot_Figure_S3b <- function(origin,nruns,doPCA){
  require(likert)
  #library(sjPlot)
  #library(sjmisc)
#  data(efc)
  
  climOsoil="soilAclimate"
  output_term="soilLat"
 # output_term="climLat"
  
  #-------------
  # load data
  #-------------
  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
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
      

      if(output_term=="soilLat"){
        colnames(m)<- c("Item","Independent_latitude","Joint","Independent_soil")
      df <- data.frame(m)
      df <- transform(df,Independent_latitude=as.numeric(as.vector(Independent_latitude)),
                      Independent_soil=as.numeric(as.vector(Independent_soil)),Joint=as.numeric(as.vector(Joint)))
      }  
      if(output_term=="climLat"){
        colnames(m)<- c("Item","Independent_climate","Joint","Independent_latitude")
       df <- data.frame(m)
       df <- transform(df,Independent_climate=as.numeric(as.vector(Independent_climate)),
                       Independent_latitude=as.numeric(as.vector(Independent_latitude)),Joint=as.numeric(as.vector(Joint)))
      }  
  
  dfs <- likert(summary = df)#,grouping = put_into_traitGroup(colnames(hp_now$r2_soil)))
  str(dfs)
  #The items are not present, but the likert object can still be summarised:
  summary(dfs)
  apply(dfs$results[put_into_traitGroup(dfs$results[,1])=="Size",],MARGIN = 2,FUN = median)
  apply(dfs$results[put_into_traitGroup(dfs$results[,1])=="Eco",],MARGIN = 2,FUN = median)
  #Plots
  
  scale_height = knitr::opts_chunk$get('fig.height')*2.5
  scale_width = knitr::opts_chunk$get('fig.width')*.7
  knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)
  theme_update(legend.text = element_text(size = rel(1)))
  
  target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                 "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")
  target_order=Rename_Vars(target_order1)[,3]

  try(dev.off())  
  out <- list()
  out$dfs <- dfs
  out$target_order1 <- target_order1
  
  pdf(file = file.path(origin,"figures","figure_S_lat",paste0("Figure_S3_",output_term,"_",climOsoils,"_.pdf")),width = 7,height = 7)
  par(mfrow=c(1,1),mar=c(8,8,2,2))
  try(plot(out$dfs,group.order = out$target_order1) + 
        ggtitle(title)+ 
        ylab("% of trait variance explained by climate and/or noise")+
        scale_fill_manual(values=c(climate_col, soil_col, "lightgray"),drop=TRUE))
  dev.off()
  
  #return(out)
  
}