Tab_S2 <- function(origin){
  
  
  ### TERMINAL:
  setwd("/Net/Groups")
  ### LOCAL
  orig_loctem =  "BGI"#"/Volumes/BGI" 
  #orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  output_term="_sel20_"#_minmaxmed
  spec_count=20 # if output_term == "_sel20_"
  NA_now="NA_mnNbr"#"NA_stays_NA"
  
  # ---------------------------------------------------------------------------------------
  # define the origin path
  origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
  
  load(file.path(origin,"data","helper_files","PCA_Fig1",paste0("PCA_agg1.RData")))
  

  res <- matrix(NA,ncol=5,nrow=17)
  colnames(res) <- c("Trait names","Trait Group", "Trait name according to TRY / TraitID",	"Unit",	"nb of observations")
  trait_names1 <- c("Leaf carbon (C) content per leaf dry mass",
               "Leaf nitrogen (N) content per leaf dry mass",
               "Leaf nitrogen (N) content per leaf area ",
               "Leaf nitrogen/phosphorus (N/P) ratio",
               "Leaf phosphorus (P) content per leaf dry mass",
               "Leaf area per leaf dry mass (specific leaf are, SLA)",
               "Stem dry mass per stem fresh volume (stem specific density, SSD, wood density)",
               "Leaf nitrogen (N) isotope signature (delta 15N)",
               "Seed number per reproduction unit",
               "Wood vessel element length",
               "Stem conduit density (vessels and tracheids)",
               "Dispersal unit length",
               "Leaf area",
               "Leaf fresh mass",
               "Plant height ",
               "Seed dry mass",
               "Seed length")
  trait_names2 <- c("LeC","LeN","LeNArea","LeNP","LeP","SLA","SSD","Led15N","SenbU","VesLen","ConduitDens",
                   "DispULen","LeArea","LeFMass","PlantHeight","SeedMass","SeLen")
  trait_id <- c(13,14,50,56,15,11,4,78,138,282,169,237,1,163,18,26,27)
  
  res[,1] <- Rename_Vars(trait_names2)[,3]
  res[,2] <- put_into_traitGroup(trait_names2)
  res[,3] <- paste0(trait_names1,"/ ",trait_id)
  res[,4] <- c("mg g-1","mg g-1","g m-2","g g-1","mg g-1","mm2 mg-1",
               "mg mm-3","???","","??m","mm-2","cm","mm2", "mg","m","mg","mm")
  for(i in 1:17){
  res[i,5] <- sum(info_2[[paste0("observation.count.",trait_names2[i])]])
  }
  res <- data.frame(res)
  #  res[2,] <- c("The ratio of the quantity (SEP:quantity) of carbon (CHEBI:carbon atom) in the leaf (PO:leaf) or component thereof, i.e. leaf lamina or leaflet (PO:leaf lamina, PO:leaflet) per respective unit dry mass (TOP:leaf dry mass, TOP:leaf lamina dry mass, TOP:leaflet dry mass)",
#               "The ratio of the quantity (SEP:quantity) of nitrogen (CHEBI:nitrogen atom) in the leaf (PO:leaf) or component thereof, i.e. leaf lamina or leaflet (PO:leaf lamina, PO:leaflet) per respective unit dry mass (TOP:leaf dry mass, TOP:leaf lamina dry mass, TOP:leaflet dry mass)",
  #               "",
  #               "",
  #               "",
  #               "",
  #13    "the area (PATO:area) of a leaf (PO:leaf) in the one sided projection")
  
  res <- res[order(res[,2]),]
  require("xlsx")
  write.xlsx(res, file=file.path(origin,"data","Results","tables","Tab_S2","Table_S2.xls"), sheetName="Sheet1", 
             col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)
  
}