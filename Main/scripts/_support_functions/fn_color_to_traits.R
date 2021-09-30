color_to_traits <- function(vector_now){
  

  
  Size.traits = c("SeedMass","Seed mass","DispULen","Dispersal U length","SeLen","Seed length",
                    "LeArea","Leaf Area","Leaf area","PlantHeight","Height","LeFMass","Leaf fresh mass","ConduitDens","Conduit density",
                    "PC1")   
  LES.traits = c("LeC","Leaf C","LeN","Leaf N","SSD","Stem Density","LeP","Leaf P","LeNP","LDMC","LeN_","LeNArea","Leaf N per Area","SLA","Leaf N:P ratio")
  Other.traits = c("SenbU","Seeds per Reprod U","VesLen","Vessel el. length","Led15N","Delta 15 N")

  out <- rep("white",length(vector_now))
  out[vector_now%in%Size.traits] <- size_col#"#5884b4" #"#0b0062" #"#5884b4"  #"#8da0cb" #"#998ec3"
  out[vector_now%in%LES.traits] <-  eco_col#"#cd6787"#"#cd6787" #"#fc8d62" #"#d95f02"
  out[vector_now%in%Other.traits] <-other_col#"#e6cf6c"  #"#e6cf6c"  #"#8da0cb" #"#998ec3"


  return(out)
}
