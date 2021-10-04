
  require("dplyr")
  
  library(vegan)
  library(ks) 
  library(stats) 
  library(grDevices)
  library(calibrate)
  
  
  ## ---------------------------------------- ##
  # read in trait data, back transform and merge
  ## ---------------------------------------- ##

  #------------------------------------------------------------------------
  # 0.1  Load data
  #------------------------------------------------------------------------
  # load predicted HPMF 
#  source(file.path(origin_prep,"scripts","_master","_01_Prepare_master_data_for_submission.R"))
#  source(file.path(origin_prep,"scripts","_master","_02_Prepare_TRY_Climate_Soil.R"))
  # load predicted HPMF 
  list.files(file.path(origin,"data","master_matrix"))
  TRY_pred_geo <- read.csv(file=file.path(origin,"data","master_matrix","TRY_pred_geo_201910022.csv"))

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
  # 0.1.2 merge with latlon downloaded from TRY-db.org
  #------------------------------------------------------------------------------------
  cat_o <- read.csv(file.path(origin,"data","helper_files","TRY","Categorical_Traits","TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.csv"))
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
  
  #------------------------------------------------------------------------------------
  # 0.2.1 rename the traits thus harmonize
  #------------------------------------------------------------------------------------
  # observed
  TRY_obs <- TRY_pred_geo[,c(5,2,4,9,10,11,20:ncol(TRY_pred_geo))]
  colnames(TRY_obs)
  dim(TRY_obs)
  nms=Rename_VarsShorttoX(names(TRY_obs))[,2]
  colnames(TRY_obs)[!is.na(nms)] <- nms[!is.na(nms)]
#  names(TRY_obs)[2] <- "AccSpeciesName"
  head(TRY_obs)
  colnames(TRY_obs)
  colnames(TRY_pred_geo)
  
  
  # rename TTT
  colnames(TTTcat)
  TTT_4 <- TTTcat[,c(51,48,2,4,6,7,63:ncol(TTTcat))]
  colnames(TTT_4)[1:3] <- c("Lat","ObservationID","AccSpeciesName")
  colnames(TTT_4)
  #  LeNArea LeNP Led15N SenbU ConduitDens DispULen VesLen
  nms = Rename_VarsTRYnametoX(colnames(TTT_4))[,2]
  cbind(colnames(TTT_4),nms)
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
  
  #nms=Rename_VarsXtoTRYname(colnames(TRY_TTT))[,2]
  nms=Rename_VarsXtoShort(colnames(TRY_TTT))[,2]
  colnames(TRY_TTT)[!is.na(nms)] <- nms[!is.na(nms)]
  
  #------------------------------------------------------------------------------------
  # 0.5 save data.
  #------------------------------------------------------------------------------------
  list.files(file.path(origin,"data"))
  
  write.csv(TRY_TTT,file.path(origin,"data","master_matrix","TTT","TRY_pred_TTT_geo17_.csv"))
  
  head(TRY_TTT)
  