
  require("dplyr")
  
  output_term = "" 
  
  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  require(FactoMineR)

  
  ## ---------------------------------------- ##
  # read in trait data, back transform and merge
  ## ---------------------------------------- ##
  
  ### TERMINAL:
  setwd("/Net/Groups")
  # JJoswig 2019_10_18
  orig_loctem =  "BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  orig_loctem =  "/Volumes/BGI" # 
  #orig_loctem =  "/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  
  origin = file.path(orig_loctem,"work_1","2018_Dichotomy","FINAL","Submission","_GapFill")
  origin_tmp = file.path(orig_loctem,"work_1","2018_Dichotomy","FINAL","Submission","00_Prepare_for_submission")
  origin_prep = "/Volumes/BGI/work_1/2018_Dichotomy/FINAL/Submission/00_Prepare_for_submission"
  origin_Agg0data =file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix/_aggregated_agg0")
  
  
  #------------------------------------------------------------------------
  # read some functions
  #------------------------------------------------------------------------
  source(file.path(origin,"scripts","_support_functions","fn_rename_variable_names.R"))
  source(file.path(origin,"scripts","_support_functions","fn_rename_vars_fromXtoTRYname.R"))
  
  change_dec <- function(input){
    input <- gsub(",", "\\.", input)
    return(input)
  }
  
  
  #------------------------------------------------------------------------
  # 0.1  Load data
  #------------------------------------------------------------------------
  # load predicted HPMF 
  
  source(file.path(origin_prep,"scripts","_master","_01_Prepare_master_data_for_submission.R"))
  source(file.path(origin_prep,"scripts","_master","_02_Prepare_TRY_Climate_Soil.R"))
  # load predicted HPMF 
  list.files(file.path(origin_Agg0data,"TRY","predicted","2015","Gap_Filled_Data_FF"))
#  TRY_pred_o <- read.csv(file.path(origin_Agg0data,"TRY","predicted",
#                                   "2015","Gap_Filled_Data_FF","mean_trait_prediction_GapFilling2015_01_31_cleaned.csv"),header = T)
#  TRY_gf <- read.csv(file=file.path(origin_Agg0data,"TRY","gap_filled","TRY_gf_201910022.csv"))
#  head(TRY_gf)
  list.files(file.path(origin_Agg0data,"TRY","predicted"))
  TRY_pred_geo <- read.csv(file=file.path(origin_Agg0data,"TRY","predicted","TRY_pred_geo_201910022.csv"))
  head(TRY_pred_geo)
  
#  # load latlot data
#  TRY_latlon <- read.table(file.path(origin_Agg0data,"TRY","auxiliary","ObsID_TraitID_AccSpecID_Lat_Lon_AccSpecName.txt"),
#                           header = T, sep="\t")
#  head(TRY_latlon)
#  dim(TRY_latlon)
  
  # load original observed TTT data
  list.files(file.path(origin,"data","TTT"))
  TTT_obs_o <- read.csv(file.path(origin,"data","TTT","TTT.csv"))
  dim(TTT_obs_o)
  TTT_obs <- TTT_obs_o
  
  #------------------------------------------------------------------------------------
  # 0.1.2 TTT prep 
  #------------------------------------------------------------------------------------
  TTT_2 <- TTT_obs[!duplicated(TTT_obs[,colnames(TTT_obs)=="IndividualID"]),1:15]
  ord <- order(TTT_2[,colnames(TTT_2)=="IndividualID"])
  for(i in 1:15){
    TTT_2[,i] <- TTT_2[,i][ord]
  }
  trait_names=unique(TTT_2[,colnames(TTT_2)=="Trait"])
  trait_names <- trait_names[trait_names!=""]
  head(TTT_2)
  
  t=1
  for(t in 1:length(trait_names)){
    
    ix <- TTT_obs[,colnames(TTT_obs)=="Trait"]==trait_names[t]
    sum(ix)
    trait_new=rep(NA,nrow(TTT_2))
    
    match(TTT_obs[ix,4],TTT_2[,4])
    vls <- match(TTT_2[,4],TTT_obs[ix,4])
    vls <- vls[!is.na(vls)]
    length(vls)
    sum(!is.na( TTT_obs[ix,colnames(TTT_obs)=="Value"]))
    
    trait_new[vls]  <-  TTT_obs[ix,colnames(TTT_obs)=="Value"]
    TTT_2 <- cbind(TTT_2,trait_new)
    colnames(TTT_2)[ncol(TTT_2)] <- trait_names[t]
  }
  head(TTT_2)
  TTT_3 <- TTT_2
  
  #------------------------------------------------------------------------------------
  # 0.1.2 merge with latlon
  #------------------------------------------------------------------------------------
  list.files(file.path(origin,"data","helper_files","Categorical_Traits"))
  cat_o <- read.csv(file.path(origin,"data","helper_files","Categorical_Traits","TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.csv"))
  head(TTT_3)
  
  # For TTT
  {
    sum(cat_o[,2]%in%TTT_3[,2])
    colnames(TTT_3)
    mtc <- match(TTT_3[,colnames(TTT_3)=="AccSpeciesName"],cat_o[,colnames(cat_o)=="AccSpeciesName"])
    mtc <- mtc[!is.na(mtc)]
    length(mtc)
    TTTcat <- cbind(cat_o[mtc,],TTT_3[TTT_3[,2]%in%cat_o[,2],])
    TTTcat <-TTTcat[,!duplicated(colnames(TTTcat))]
    
    ix_unknown<- !TTT_3[,colnames(TTT_3)%in%"AccSpeciesName"]%in%TTTcat[,colnames(TTTcat)%in%"AccSpeciesName"]
    sum(ix_unknown)
    sp_l <- unique(TTT_3[ix_unknown,colnames(TTT_3)%in%"AccSpeciesName"])
    sp_lc <- cat_o[,colnames(cat_o)%in%"AccSpeciesName"]
    
    print(paste(nrow(TTTcat)," entries are available in cat information"))
    print(paste("missing thus ", nrow(TTT_3)-nrow(TTTcat)," information for individuals"))
    # extent the categorical information.
    print(paste0("Or ", length(sp_l)," species."))
    #--------------------------------------------------------------------------------
    # in order to increase the matches for taxonomy:
    # create cat & search with "first name" only
    #--------------------------------------------------------------------------------
    sp_l2<- strsplit(sp_l, " ")
    sp_lc2<- strsplit(sp_lc, " ")
    sp_lc3=NA
    i=1
    # split and put into vector
    for(i in 1:length(sp_lc2)){sp_lc3 <- c(sp_lc3,sp_lc2[[i]][1])}
    sp_lc3 <- sp_lc3[-1]
    testInteger <- function(x){
      test <- all.equal(x, as.integer(x), check.attributes = FALSE)
      if(test == TRUE){ return(TRUE) }
      else { return(FALSE) }
    }
    
    length(sp_lc2)
    nrow(cat_o)
    
    cat2 <- cat_o
    i=1
    for(i in 1:length(sp_l)){
      print(i)
      try(rm("res_sp"))
      res_sp <- c(NA,which(sp_lc3%in%sp_l2[[i]][1]))
      sp_lc3[res_sp]
      
      if(length(res_sp)>1){
        print(sp_l[i])
        cat_new_all <- cat_o[res_sp,]
        cat_new <- cat_new_all[which(!is.na(cat_new_all[,2]))[1],]
        cat_new[colnames(cat_o)%in%"AccSpeciesName"] <- sp_l[i]
        cat2 <- rbind(cat2,cat_new)
        tail(cat2)
        print(sp_l[i])
      }
    }
    
    dim(cat_o)
    dim(cat2)
    
    # merge again :)
    mtc <- match(TTT_3[,2],cat2[,2])
    mtc <- mtc[!is.na(mtc)]
    length(mtc)
    TTTcat <- cbind(cat2[mtc,],TTT_3[TTT_3[,2]%in%cat2[,2],])
    TTTcat <- TTTcat[,!duplicated(colnames(TTTcat))]
    
    print(paste("we have ", nrow(TTTcat)," matching species."))
    print(paste("missing thus ", nrow(TTT_3)-nrow(TTTcat)," species."))
    
    # detect which specie are missing
    ix_unknown <- !TTT_3[,colnames(TTT_3)%in%"AccSpeciesName"]%in%TTTcat[,colnames(TTTcat)%in%"AccSpeciesName"]
    sp_l <- unique(TTT_3[ix_unknown,colnames(TTT_3)%in%"AccSpeciesName"])
    sp_l
    
    # do them by hand and Wiki ;) or plant 
    cat_new <- matrix(NA,nrow=38,ncol(cat2))
    cat_new[2,c(2,4,6,7)] <- c("Scorzoneroides helvetica","Scorzoneroides","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[3,c(2,4,6,7)] <- c("Beckwithia glacialis","Beckwithia","Ranunculaceae","Angiosperm_Magnoliid")
    cat_new[4,c(2,4,6,7)] <- c("Scorzoneroides autumnalis","Scorzoneroides","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[5,c(2,4,6,7)] <- c("Saussurea angustifolium","Saussurea","Asteraceae","Angiosperm_Eudicotyl")#typo
    # update name
    sp_l[5] <- "Saussurea angustifolium"
    TTT_3[TTT_3[,colnames(TTT_3)=="AccSpeciesName"]=="Saussarea angustifolium",colnames(TTT_3)=="AccSpeciesName"] <- "Saussurea angustifolium"
    cat_new[6,c(2,4,6,7)] <- c("Coronidium scorpioides","Coronidium","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[7,c(2,4,6,7)] <- c("Austrolopyrum velutinum"," Eucalyptus","Myrtaceae","Angiosperm_Eudicotyl")# called "Eucalyptus delegatensis" From Forest to Fjaeldmark; Edited by S. Harris and A. Kitchener ISBN 0 7246 6364 9.
    cat_new[8,c(2,4,6,7)] <- c("Argyrotegium fordianum","Argyrotegium","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[9,c(2,4,6,7)] <- c("Acrothamnus montanus","Argyrotegium","Asteraceae","Angiosperm_Eudicotyl")# called "Lissanthe montana" Plant list
    cat_new[10,c(2,4,6,7)] <- c("Mniarum biflorum","Mniarum","Caryophyllaceae","Angiosperm_Eudicotyl")
    cat_new[11,c(2,4,6,7)] <- c("Abrotanella inconspicua","Abrotanella","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[12,c(2,4,6,7)] <- c("Anisotome flexuosa","Anisotome","Apiaceae","Angiosperm_Eudicotyl")
    cat_new[13,c(2,4,6,7)] <- c("Anisotome imbricata","Anisotome","Apiaceae","Angiosperm_Eudicotyl")
    cat_new[14,c(2,4,6,7)] <- c("Argyrotegium mackayi","Argyrotegium","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[15,c(2,4,6,7)] <- c("Hectorella caespitosa","Hectorella","Montiaceae","Angiosperm_Eudicotyl")
    cat_new[16,c(2,4,6,7)] <- c("Phyllachne rubra","Phyllachne","Stylidiaceae","Angiosperm_Eudicotyl")# synonyme: Helophyllum rubrum Hook.f.
    cat_new[17,c(2,4,6,7)] <- c("Abrotanella caespitosa","Abrotanella","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[18,c(2,4,6,7)] <- c("Anaphalioides bellidioides","Anaphalioides","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[19,c(2,4,6,7)] <- c("Anisotome aromatica","Anisotome","Apiaceae","Angiosperm_Eudicotyl")
    cat_new[20,c(2,4,6,7)] <- c("Ourisia caespitosa","Ourisia","Plantaginaceae","Angiosperm_Eudicotyl")
    cat_new[21,c(2,4,6,7)] <- c("Ourisia glandulosa","Ourisia","Plantaginaceae","Angiosperm_Eudicotyl")
    cat_new[22,c(2,4,6,7)] <- c("Phyllachne colensoi","Phyllachne","Stylidiaceae","Angiosperm_Eudicotyl")
    cat_new[23,c(2,4,6,7)] <- c("Dichosciadium ranunculaceum","Dichosciadium","Apiaceae","Angiosperm_Eudicotyl")
    cat_new[24,c(2,4,6,7)] <- c("Argyrotegium nitidulum","Argyrotegium","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[25,c(2,4,6,7)] <- c("Ewartia nubigena","Ewartia","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[26,c(2,4,6,7)] <- c("Parantennaria uniceps","Parantennaria","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[27,c(2,4,6,7)] <- c("Schizeilema fragoseum","Schizeilema","Apiaceae","Angiosperm_Eudicotyl")
    cat_new[28,c(2,4,6,7)] <- c("Eutrema edwardsii","Eutrema","Brassicaceae","Angiosperm_Eudicotyl")
    cat_new[29,c(2,4,6,7)] <- c("Puccinellia sp.","Puccinellia","Poaceae","Angiosperm_Eudicotyl")#typo it is Puccinellia
    # update name
    sp_l[29] <- "Puccinellia sp."
    TTT_3[TTT_3[,colnames(TTT_3)=="AccSpeciesName"]=="Puccinelia sp.",colnames(TTT_3)=="AccSpeciesName"] <- "Puccinellia sp."
    cat_new[30,c(2,4,6,7)] <- c("Bellidastrum michelii","Bellidastrum","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[31,c(2,4,6,7)] <- c("Glandora diffusa","Glandora","Boraginaceae","Angiosperm_Eudicotyl")
    cat_new[32,c(2,4,6,7)] <- c("Dethawia splendens","Dethawia","Apiaceae","Angiosperm_Eudicotyl")
    cat_new[33,c(2,4,6,7)] <- c("Chionophila jamesii","Chionophila","Plantaginaceae","Angiosperm_Eudicotyl")
    cat_new[34,c(2,4,6,7)] <- c("Tonestus pygmaeus","Tonestus","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[35,c(2,4,6,7)] <- c("Anticlea elegans","Anticlea","Melanthiaceae","Angiosperm_Eudicotyl")
    cat_new[36,c(2,4,6,7)] <- c("Psephellus caucasicus","Psephellus","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[37,c(2,4,6,7)] <- c("Cyanus cheiranthifolius","Cyanus","Asteraceae","Angiosperm_Eudicotyl")
    cat_new[38,c(2,4,6,7)] <- c("Iranecio taraxacifolius","Iranecio","Asteraceae","Angiosperm_Eudicotyl")
    
    colnames(cat_new) <- colnames(cat2)
    cat3 <- rbind(cat2,cat_new)
    
    #cat[cat[,6]=="Ranunculaceae",c(4,6,7)]
    #cat[cat[,4]=="Scorzoneroides",c(4,6,7)]
    #cat[cat[,2]=="Saussurea angustifolium",c(2,4,6,7)]
    
    # merge last time
    mtc <- match(TTT_3[,2],cat3[,2])
    mtc <- mtc[!is.na(mtc)]
    length(mtc)
    TTTcat <- cbind(cat3[mtc,],TTT_3[TTT_3[,2]%in%cat3[,2],])
    TTTcat <- TTTcat[,!duplicated(colnames(TTTcat))]
  }
  
  print(paste("we have ", nrow(TTTcat)," matching species."))
  print(paste("missing thus ", nrow(TTT_3)-nrow(TTTcat)," species."))
  
  colnames(TTTcat)
  sum(complete.cases(TTTcat[,c(2,4,6,7)]))==nrow(TTTcat)
  TTTmissing <- TTTcat[!complete.cases(TTTcat[,c(2,4,6,7,51)]),]
  unique(TTTmissing[,2])
  
  ix_unknown <- !TTT_3[,colnames(TTT_3)%in%"AccSpeciesName"]%in%TTTcat[,which(colnames(TTTcat)%in%"AccSpeciesName")[1]]
  sp_l <- unique(TTT_3[ix_unknown,colnames(TTT_3)%in%"AccSpeciesName"])
  sp_l
  colnames(TTTcat)
  
  # TRY_latlon <- read.table(file.path(origin_Agg0data,"TRY","auxiliary","ObsID_TraitID_AccSpecID_Lat_Lon_AccSpecName.txt"),
  #                          header = T, sep="\t")
  # # check & change mode:
  # TRY_latlon$AccSpeciesName = as.character(TRY_latlon$AccSpeciesName)
  # # cut out duplicated obs IDs
  # TRY_latlon2 <- TRY_latlon[,-which(names(TRY_latlon)%in%c("TraitID"))]
  # TRY_latlon2 <- TRY_latlon2[!duplicated(TRY_latlon2$ObservationID),]
  # dim(TRY_latlon2)
  # # merge with predicted 
  # TRY_pred_geo <- merge(x = TRY_latlon2,y = TRY_gf, by="ObservationID")
  
  # For TRY
  #dim(TRY_pred_geo)
  
  #------------------------------------------------------------------------------------
  # 0.2.1 rename 
  #------------------------------------------------------------------------------------
  #observed
  colnames(TRY_pred_geo)
  
  TRY_obs <- TRY_pred_geo[,c(5,2,4,9,10,11,20:ncol(TRY_pred_geo))]
  colnames(TRY_obs)
  dim(TRY_obs)
  nms=Rename_VarsShorttoX(names(TRY_obs))[,2]
  colnames(TRY_obs)[!is.na(nms)] <- nms[!is.na(nms)]
#  names(TRY_obs)[2] <- "AccSpeciesName"
  head(TRY_obs)
  
  
  # rename TTT
  colnames(TTTcat)
  TTT_4 <- TTTcat[,c(51,48,2,4,6,7,63:ncol(TTTcat))]
  colnames(TTT_4)[1:3] <- c("Lat","ObservationID","AccSpeciesName")
  colnames(TTT_4)
  nms = Rename_VarsTRYnametoX(colnames(TTT_4))[,2]
  colnames(TTT_4)[!is.na(nms)] <- nms[!is.na(nms)]
  colnames(TTT_4)
  TTT_4[,2] <- paste0(TTT_4[,2],"_ttt")
  
  #------------------------------------------------------------------------------------
  # 0.3 MERGE TRY and TTT
  #------------------------------------------------------------------------------------
  apply(!is.na(TTT_4),2,sum)
  TTT_5 <- matrix(NA,ncol=c(5+length(unique(colnames(TRY_obs)[6:ncol(TRY_obs)],
                                            colnames(TTT_4)[6:ncol(TTT_4)]))),
                  nrow=nrow(TTT_4))
  colnames(TTT_5) <- c(colnames(TTT_4)[1:5],
                       unique(colnames(TRY_obs)[6:ncol(TRY_obs)],
                              colnames(TTT_4)[6:ncol(TTT_4)]))
  
  i=1
  trait_common <- colnames(TTT_5)[colnames(TTT_5)%in%colnames(TTT_4)]
  for(i in 1:length(trait_common)){
    TTT_5[,colnames(TTT_5)%in%trait_common[i]] <- TTT_4[,colnames(TTT_4)%in%trait_common[i]]
    
    print(colnames(TTT_5)[colnames(TTT_5)%in%trait_common[i]])
    print(colnames(TTT_4)[colnames(TTT_4)%in%trait_common[i]])
  }
  
  colnames(TTT_4)[colnames(TTT_4)%in%colnames(TTT_5)]
  
  sum(colnames(TTT_5)==colnames(TRY_obs))==ncol(TRY_obs)
  TTT_5[,c(1:ncol(TTT_5))]
  rowSums(!is.na(TRY_obs[,c(6:ncol(TRY_obs))]))
  rowSums(!is.na(TTT_5[,c(6:ncol(TTT_5))]))
  
  TTT_5[!complete.cases(TTT_5[,1:5]),c(1:5)]
  TRY_obs[!complete.cases(TRY_obs[,1:5]),c(1:5)]
  TRY_obs[!complete.cases(TRY_obs[,6:ncol(TRY_obs)]),6:ncol(TRY_obs)]
  
  TRY_TTT <- rbind(TRY_obs,TTT_5)
  
  nms=Rename_VarsXtoTRYname(colnames(TRY_TTT))[,2]
  nms=Rename_VarsXtoShort(colnames(TRY_TTT))[,2]
  colnames(TRY_TTT)[!is.na(nms)] <- nms[!is.na(nms)]
  
  #------------------------------------------------------------------------------------
  # 0.5 save data.
  #------------------------------------------------------------------------------------
  list.files(file.path(origin,"data"))
  
  write.csv(TRY_TTT,file.path(origin,"data","TRY_pred_TTT_geo17.csv"))
  
  
  trait_agg1_2 <- as.data.frame(TRY_TTT[,7:ncol(TRY_TTT)])
  lat_agg1_2 <- as.numeric(TRY_TTT[,colnames(TRY_TTT)=="Lat"])
  
  # binning needed
  lat_bins <- round(as.numeric(lat_agg1_2),0)
  data_trait <-cbind(lat_bins,trait_agg1_2) 
  data_trait <- as.data.frame(data_trait)

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
  
  origin_figures <- "/Volumes/BGI/work_1/2018_Dichotomy/FINAL/Submission/01_Complete_for_submission"

  if(!dir.exists(file.path(origin_figures,"figures","figure_S_latTTT_review"))){
    dir.create(file.path(origin_figures,"figures","figure_S_latTTT_review"))}
  
  #----------------------------------------------------------------------------
  # plot the PC1 against absolute latitude
  #----------------------------------------------------------------------------
  { 
    pdf(file=file.path(origin_figures,"figures","figure_S_latTTT_review","figure_S_latTTT_review.pdf"),width = 20,height = 8)
    t=1
    for(t in 1:ncol(data_trait)){
    par(mfrow=c(1,1),mar=c(5,5,2,2))
    plot(abs(lat_agg1_2),log(as.numeric(data_trait[,t+1])),frame.plot = FALSE,
         xlim=c(0,110),col=add.alpha(col = color_to_traits(colnames(data_trait)[t+1]),alpha = .5),xaxt="n",
         ylab=paste0(names(trait_agg1_2)[t+1]," [log]"),
         xlab="Latitude (abs)",pch=16,cex=.7,cex.lab=2.5,cex.axis=2)
    axis(1,at = seq(from=0,to = 110,by = 10),labels = c(0,10,20,30,40,50,60,70,80,"","",""),tick = F,cex=2)
    points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=2.5,
           col="white",pch=16)
    points(data_trait_s$Group.1,log(data_trait_s[,t+1]),cex=1.5,
           col="black",pch=16)
    }
    dev.off()
    
  }
  
  }
  }
