setwd("/Net/Groups")
# JJoswig 2019_10_18
orig_loctem =  "BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
#orig_loctem =  "/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
origin_Agg0data =file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix/_aggregated_agg0")

list.files(file.path(origin_Agg0data,"TRY","predicted","2015","Gap_Filled_Data_FF"))

#------------------------------------------------------------------------
# read some functions
#------------------------------------------------------------------------
source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_Rename_Vars.R"))
trait_names <- c("LeArea","Led15N","LeNArea","LeNP","LeC","LeP","LeN","SLA","ConduitDens",
                 "LeFMass","SenbU","PlantHeight","SeLen","VesLen","DispULen","SeedMass","SSD")

list.files(file.path(origin_Agg0data,"TRY","predicted","2015"))
  TRY_pred <- read.csv(file.path(origin_Agg0data,"TRY","predicted",
                                "2015","Gap_Filled_Data_FF","GapFilling2015_01_31_cleaned.csv"),header = T)
  TRY <- TRY_pred[,Rename_Vars(names(TRY_pred))[,2]=="trait"]
  names(TRY) <- Rename_Vars(names(TRY))[,3]
  #-----------------------------------------------
  # TRY_geo <- read.csv(file.path(origin_Agg0data,"TRY","predicted",
  #                               "2015","GapFilling2015_01_31_geo.csv"),header = T)
  #TRY <- TRY_geo # OR
  #-----------------------------------------------
  # read in the data before in Masterscript
  info_2
  #----------------------------------------------
dim(TRY)
names(TRY)
sum(rowSums(!is.na(TRY))>0)

  m <- matrix(NA,ncol=9,nrow=length(trait_names))  
  colnames(m) <- c("nb_observed","%observed","nb_observedStudy","%_observedStudy","Species","PlantGrowthForm","median number of jointly measured",
                   "trait group","trait name")
  rownames(m) <- trait_names
  
  i=1
  for( i in 1:length(trait_names)){
    colm=which(names(TRY)%in%trait_names[i])
    print(colnames(TRY)[colm])
  m[i,1] <- sum(!is.na(TRY[,colm]))
  m[i,2] <- round(sum(!is.na(TRY[,colm]))/nrow(TRY)*100,digits=2)
  m[i,4] <- length(unique(TRY$Species[!is.na(TRY[,colm])]))
  m[i,5] <- length(unique(TRY$Genus[!is.na(TRY[,colm])]))
  m[i,6] <- length(unique(TRY$PlantGrowthForm[!is.na(TRY[,colm])]))
  m[i,8] <- put_into_traitGroup(names(TRY)[i])
  m[i,9] <- Rename_Vars(names(TRY)[i])[,3]
  ix <- !is.na(TRY[,colm])
#  m[i,6] <- median(rowSums(!is.na(TRY[ix,which(names(TRY)%in%trait_names)])))
  }
#  rownames(m)[Rename_Vars(rownames(m))[,3]!="other"] <-  Rename_Vars(rownames(m))[,3][Rename_Vars(rownames(m))[,3]!="other"]
  #m2 <- m[!(1:nrow(m)%in%grep(x = rownames(m),pattern = "X")),] 
  m2 <- m
  m2[which(rownames(m2)%in%"SSD"),3] <- sum(info_2$observation.count.SSD)
  m2[which(rownames(m2)%in%"SLA"),3] <- sum(info_2$observation.count.SLA)
  m2[which(rownames(m2)%in%"LeC"),3] <- sum(info_2$observation.count.LeC)
  m2[which(rownames(m2)%in%"LeN"),3] <- sum(info_2$observation.count.LeN)
  m2[which(rownames(m2)%in%"LeP"),3] <- sum(info_2$observation.count.LeP)
  m2[which(rownames(m2)%in%"LeNArea"),3] <- sum(info_2$observation.count.LeNArea)
  m2[which(rownames(m2)%in%"LeNP"),3] <- sum(info_2$observation.count.LeNP)
  m2[which(rownames(m2)%in%"Led15N"),3] <- sum(info_2$observation.count.Led15N)
  m2[which(rownames(m2)%in%"SenbU"),3] <- sum(info_2$observation.count.SenbU)
  m2[which(rownames(m2)%in%"VesLen"),3] <- sum(info_2$observation.count.VesLen)
  m2[which(rownames(m2)%in%"LeArea"),3] <- sum(info_2$observation.count.LeArea)
  m2[which(rownames(m2)%in%"PlantHeight"),3] <- sum(info_2$observation.count.PlantHeight)
  m2[which(rownames(m2)%in%"SeedMass"),3] <- sum(info_2$observation.count.SeedMass)
  m2[which(rownames(m2)%in%"SeLen"),3] <- sum(info_2$observation.count.SeLen)
  m2[which(rownames(m2)%in%"LeFMass"),3] <- sum(info_2$observation.count.LeFMass)
  m2[which(rownames(m2)%in%"ConduitDens"),3] <- sum(info_2$observation.count.ConduitDens)
  m2[which(rownames(m2)%in%"DispULen"),3] <- sum(info_2$observation.count.DispULen)
  m2 <- as.matrix(m2)
  m2[,4] <- round(as.numeric(m2[,3])/sum(info_2$observation.count.tot.1)*100,digits=2)
  
  plot(m2[,c(1,3)])
  text(m2[,c(1,3)],labels = rownames(m2))
  abline(0,1)
  m3 <- m2[order(m2[,8]),c(9,8,1,2,3,4)]
  #write.csv(m,file=file.path(origin,"data","Results","tables","Trait_values_observed_geo.csv"))
  require("xlsx")
  write.xlsx(m3, file=file.path(origin,"data","Results","tables","Table_S6_GapFilling.xls"), sheetName="Sheet1", 
             col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)

  
  dim(TRY)
  
  