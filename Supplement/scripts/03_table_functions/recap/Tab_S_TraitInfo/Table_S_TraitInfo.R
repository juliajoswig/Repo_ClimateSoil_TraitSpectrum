tab_S_TraitInfo <- function(origin,info){
  
  library(xtable)

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
               "mg mm-3","","","m","mm-2","cm","mm2", "mg","m","mg","mm")
  for(i in 1:17){
  res[i,5] <- sum(info[[paste0("observation.count.",trait_names2[i])]])
  }
  res <- data.frame(res)
  ix <- match(target_order,res[,1])
  res <- res[ix,]
  
  if(!file.exists(file.path(origin, "tables"))){
    dir.create(file.path(origin, "tables"))}
  if(!file.exists(file.path(origin, "tables","Table_S_TraitInfo"))){
    dir.create(file.path(origin, "tables","Table_S_TraitInfo"))}
  
  write.csv(res, file=file.path(origin,"data","Results","tables","Table_S_TraitInfo","Table_S2.csv"))
  print(xtable(res, type = "latex"), file =file.path(origin,"data","Results","tables","Table_S_TraitInfo","Table_S2.tex"))
  print(xtable(res, type = "latex"))
  return(res)
}
