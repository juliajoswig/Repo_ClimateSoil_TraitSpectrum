put_into_traitGroup <- function(vector_now){
  
  nm <- c("LeArea","SSD","SLA","LeC","LeN","LeP","PlantHeight", "SeedMass",   
  "SeLen", "LDMC","LeNArea","LeNP","Led15N","SenbU", "LeFMass","ConduitDens",
  "DispULen","VesLen","NCratio","PCratio","NPratio","Wood10","PPath10","SeedInvest" ,"PC1","PC2","PC3","PC4")
  colors_now <- c("wheat","springgreen2","royalblue1","sienna3","paleturquoise","orangered2","orange2",
                  "mediumseagreen","khaki2","indianred4","blueviolet","bisque","blanchedalmond","aquamarine2",
                  "darkolivegreen1","darkmagenta","cyan2","burlywood","antiquewhite4","blue3","coral3","hotpink","darkorange1","#771155", "#AA4488", "#CC99BB", "#114477")
  
  tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", 
                  "#117744", 
                  "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77",
                  "#AA4455", "#DD7788","red","green","aquamarine2",
                  "darkolivegreen1","darkmagenta","cyan2","burlywood","antiquewhite4","blue3","coral3","hotpink")
  
  #loadings <- read.csv(file.path(origin,"scripts","analysis","PCA_of_subgroups","Agg2_weights.csv"))
  #rownames(loadings) <- trait.names
  #rownames(loadings)[abs(loadings[,2])>.45]
  Size.traits = c("SeedMass","DispULen","SeLen","DispULen",
                    "LeArea","PlantHeight","LeFMass","ConduitDens",
                    "PC1")   
  LES.traits = c("LeC","LeN","SSD","LeP","LeNP","LDMC","LeN_","LeNArea","SLA")
  Other.traits = c("SenbU","VesLen","Led15N")
  #trait.names2[-unique(c(which(trait.names2%in%Height.traits),which(trait.names2%in%Nutrient.traits),which(trait.names2%in%Seed.traits)))]
  
  out <- rep("white",length(vector_now))
  out[vector_now%in%Size.traits] <- "Size"  #"#8da0cb" #"#998ec3"
  out[vector_now%in%LES.traits] <-  "Eco" #"#fc8d62" #"#d95f02"
  out[vector_now%in%Other.traits] <- "Other"  #"#8da0cb" #"#998ec3"
  
  return(out)
}
