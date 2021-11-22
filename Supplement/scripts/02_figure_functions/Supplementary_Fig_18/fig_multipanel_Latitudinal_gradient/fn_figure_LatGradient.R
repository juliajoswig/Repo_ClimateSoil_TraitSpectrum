
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


if(!file.exists(file.path(origin,"figures","Supplement_Fig_18"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_18"))}
if(!file.exists(file.path(origin,"figures","Supplement_Fig_18","LatGradient"))){
  dir.create(file.path(origin,"figures","Supplement_Fig_18","LatGradient"))}


  load(file = file.path(origin,"data","master_matrix","X1.RData"))
  load(file.path(origin,"data","helper_files","Ecoregions_selected","Ecoregion_Agg2.RData"))
  
  # cut to relevant Ecoregions:
  TRY_Env1=TRY_Env1[TRY_Env1$Group.2%in%Ecoregion_Agg2$ECO_NAME,]
  
  trait_agg1_2 <- as.data.frame(TRY_Env1[,which(Rename_Vars(gsub(colnames(TRY_Env1),pattern = "_gf",replacement = ""))[,2]=="trait")
                                   ])
  names(trait_agg1_2) <- gsub(names(trait_agg1_2),pattern = "_gf",replacement = "")
  lat_agg1_2 <- data.frame(Lat=TRY_Env1[,which(colnames(TRY_Env1)%in%"Lat") ])
  info <- data.frame(BIOME=TRY_Env1[,which(colnames(TRY_Env1)%in%"BIOME") ])
  
  names(lat_agg1_2) <- "Lat"
  
  # binning needed
  lat_bins <- round(lat_agg1_2,0)
  data_trait <-cbind(lat_bins,trait_agg1_2) 
  data_trait <- as.data.frame(data_trait)
  names(data_trait)[1] <- "lat_bins"
  
  new.median.fun  <- function(x) {
    if (is.numeric(x)) {
      return(median(x, na.rm=T))
    } else {
      return(unique(x))
    }
  }
  
  data_trait_ag <- aggregate(x=data_trait[which(!(names(data_trait) %in% c("lat_bins")))], 
                           by = list(data_trait$lat_bins), FUN=new.median.fun)
  data_trait_s <- data_trait_ag[order(data_trait_ag$Group.1),]
  
  if(!file.exists(file.path(origin,"figures","Supplement_Fig_18_33"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_18_33"))
  }
  if(!file.exists(file.path(origin,"figures","Supplement_Fig_18_33","LatGradient"))){
    dir.create(file.path(origin,"figures","Supplement_Fig_18_33","LatGradient"))
  }
  
  t=3
  for(t in 1:17){
  png(file.path(origin, "figures","Supplement_Fig_18_33","LatGradient",
                paste0(names(trait_agg1_2)[t],"_latgradient.png")),width=800,height=500)
  
  par(mfrow=c(1,1),mar=c(7,7,2,2))
  plot(abs(lat_agg1_2$Lat),log(data_trait[,t+1]),frame.plot = FALSE,
       xlim=c(0,110),col=add.alpha(color_to_biomes(info$BIOME),alpha = .5),xaxt="n",
       ylab="",
       xlab="",pch=16,cex=.7,cex.lab=3.5,cex.axis=2)
  axis(1,at = 40,labels = "Latitude (abs)",cex=3.5,cex.axis=3.5,line = 3,tick=FALSE)
  axis(2,at = mean(log(data_trait_s[,t+1])),
       labels = paste0(names(trait_agg1_2)[t]," [log]"),cex=3.5,cex.axis=3.5,line = 3,tick=FALSE)
  axis(1,at = seq(from=0,to = 110,by = 10),labels = c(0,10,20,30,40,50,60,70,80,"","",""),tick = F,cex=2)
  points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=2.5,
         col="white",pch=16)
  points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=1.5,
         col="black",pch=16)
  legend("topright", name_to_biomes(sort(unique(info$BIOME))),pch=16,
         col=color_to_biomes(sort(unique(info$BIOME))), border = FALSE,
        text.col = color_to_biomes(sort(unique(info$BIOME))),cex=.8,bty = "n")
  dev.off()
  }
  
  
  