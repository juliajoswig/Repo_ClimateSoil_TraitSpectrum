
# ---------------------------------------------------------------------------------------
# 01. define the origin path
# ---------------------------------------------------------------------------------------
# origin = # please add your local path here & comment the ones below.
# origin = "/Users/jjoswig/Documents/_docs/03_projects/2021/002_Dichotomy/_script_data/20210907_Script_data/Supplement" # please add your local path here 
list.files(file.path(origin,"scripts/_master"))

# load functions
source(file.path(origin,"scripts" ,"_master","fn_functions.R"))
# packages
source(file.path(origin,"scripts" ,"_master","fn_packages.R"))

nruns=50
output_term="woody"
#output_term="non_woody"
doPCA=TRUE
climOsoil="soilAclimate"

if(!file.exists(file.path(origin,"figures","Supplement_Fig_12"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_12"))}

#plot_Figure_Bias <- function(origin,nruns,doPCA,climOsoil,output_term){
  
  require(likert)
  #-------------
  # load data
  #-------------
  target_order1=c("SeLen","DispULen","SeedMass","PlantHeight","LeFMass","LeArea","ConduitDens",
                  "SSD","LeC","LeNP","LeP","LeNArea","SLA","LeN", "VesLen","Led15N","SenbU")

list.files(file.path(origin, "data", "_results","HierarchicalPartitioning","_RidgeRegression_results")) 
list.files(file.path(origin, "data", "_results","RidgeRegression")) 

  if(doPCA){pca_term="doPCA"}
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",
                   paste0("HP_",nruns,"nruns_", output_term,pca_term,".RData")))
    hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]
    title <- ""
    

    
  # data processing
      m <- matrix(NA,ncol=4,nrow=ncol(hp_now[[3]]))
      m[,1] <- colnames(hp_now[[3]])
      m[,2] <- colMeans(hp_now[[2]] - hp_now[[4]])*100# pure indep effect
      m[,3] <- colMeans(hp_now[[3]])*2*100# joint effect
      m[,4] <- colMeans(hp_now[[1]] - hp_now[[3]])*100# pure indep effect
      m[m<0]  = 0
      

        colnames(m)<- c("Item","Independent_climate","Joint","Independent_soil")
        df <- data.frame(m)
        df <- transform(df,Independent_climate=as.numeric(as.vector(Independent_climate)),
                        Independent_soil=as.numeric(as.vector(Independent_soil)),
                        Joint=as.numeric(as.vector(Joint)))

  # rename & order
  ix <- match(target_order1,df$Item)
  df$Item[ix]
  df <- df[ix,]
  df$Item <- Rename_Vars(df$Item)[,3]
        
  dfs <- likert(summary = df)
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
  out$target.order <-  Rename_Vars(target_order1)[,3]
  
  if(output_term=="non_woody"){pdf(file=file.path(origin,"figures","Supplement_Fig_12",paste0("Supplement_Fig_12a.pdf")))  }
  if(output_term=="woody"){pdf(file=file.path(origin,"figures","Supplement_Fig_12",paste0("Supplement_Fig_12b.pdf")))  }
  
  par(mfrow=c(1,1),mar=c(8,8,2,2))
  
  try(plot(out$dfs,group.order = out$target.order) + 
          ggtitle(title)+ 
          ylab("% of trait variance explained by climate and/or soil")+
          scale_fill_manual(values=c(climate_col, "lightgray", soil_col),
                            drop=FALSE))
  dev.off()

