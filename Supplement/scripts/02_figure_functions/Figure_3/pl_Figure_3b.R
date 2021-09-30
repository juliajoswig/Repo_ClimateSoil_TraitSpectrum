#install.packages("likert")
#whichfig="3b"
plot_Figure_3b <- function(origin,nruns,doPCA){
  require(likert)
  #library(sjPlot)
  #library(sjmisc)
#  data(efc)
  
  climOsoil="soilAclimate"
  output_term=""
  #-------------
  # load data
  #-------------

  if(doPCA){pca_term="doPCA"}
  if(!doPCA){pca_term="no_PCA"}
  list.files(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results")) 
  
  target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                  "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                   paste0("HP_",nruns,"nruns_",pca_term,".RData")))
  hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]
  title <- "" 
  
  # rename & order
  ix <- match(target_order1,colnames(hp_now[[1]]))
  colnames(hp_now[[1]]) <- Rename_Vars(colnames(hp_now[[1]]))[,3]
  colnames(hp_now[[2]]) <- Rename_Vars(colnames(hp_now[[2]]))[,3]
  colnames(hp_now[[3]]) <- Rename_Vars(colnames(hp_now[[3]]))[,3]
  
  hp_now[[1]] <- hp_now[[1]][,ix]
  hp_now[[2]] <- hp_now[[2]][,ix]
  hp_now[[3]] <- hp_now[[3]][,ix]
  
  
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
  
  dfs <- likert(summary = df)#,grouping = put_into_traitGroup(colnames(hp_now$r2_soil)))
  str(dfs)
  #The items are not present, but the likert object can still be summarised:
  summary(dfs)
  #Plots
  
  scale_height = knitr::opts_chunk$get('fig.height')*2.5
  scale_width = knitr::opts_chunk$get('fig.width')*.7
  knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)
  theme_update(legend.text = element_text(size = rel(1)))
  
  
  try(dev.off())  
  out <- list()
  out$dfs <- dfs
  out$target_order <-  Rename_Vars(target_order1)[,3]
  out$target_order1 <-  target_order1
  
  require(likert)
  
  pdf(file=file.path(origin,"figures","figure_3",paste0("figure_3b_R",".pdf")))
  par(mfrow=c(1,1),mar=c(8,8,2,2))
  try(plot(out$dfs,group.order = out$target_order) + 
        ggtitle("")+ 
        #          theme(axis.text=element_text(size=12))+#
        theme(axis.text.y = element_text(size=17,colour = c(color_to_traits(out$target_order1)[17:1])))+
        ylab("% of trait variance explained by climate and/or soil")+
        scale_fill_manual(values=c(climate_col, soil_col, "lightgray"),drop=TRUE))
  dev.off()
  
#  5 <- likert(tmp)
#  plot(Q5, ordered=FALSE)  + theme(aspect.ratio=0.3, legend.text =element_text(color="black",size=8), axis.text=element_text(color="black",size=12))

  
  }
