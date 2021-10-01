#install.packages("likert")
#whichfig="3b"
plot_Figure_3b <- function(origin,nruns,doPCA){
  require(likert)
  #library(sjPlot)
  #library(sjmisc)
#  data(efc)
  
  climOsoil="soilAclimate"
  output_term=""
  nruns=50
  doPCA=TRUE
  #-------------
  # load data
  #-------------

  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  list.files(file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results")) 
  
  target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                  "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")
  load(file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results",
                   paste0("HP_",nruns,"nruns_",pca_term,".RData")))
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
  
  dfs <- likert(summary = df)#,grouping = put_into_traitGroup(colnames(hp_now$r2_soil)))
  str(dfs)
  #The items are not present, but the likert object can still be summarised:
  summary(dfs)
  #Plots
  
  scale_height = knitr::opts_chunk$get('fig.height')*2.5
  scale_width = knitr::opts_chunk$get('fig.width')*.7
  knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)
  theme_update(legend.text = element_text(size = rel(1)))
  
  
  if(!dir.exists(file.path(origin,"figures","figure_3"))){
    dir.create(file.path(origin,"figures","figure_3"))}
  
  try(dev.off())  
  out <- list()
  out$dfs <- dfs
  out$target_order <-  Rename_Vars(target_order1)[,3]
  out$target_order1 <-  target_order1
  
  return(out)
  

  
  }
