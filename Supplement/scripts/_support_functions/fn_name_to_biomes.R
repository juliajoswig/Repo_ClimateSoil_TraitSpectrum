name_to_biomes <- function(vector_with_cats){
  
  Biome.names <- c("Tropical Subtropical Moist Broadleaf Forests","Tropical Subtropical Dry Broadleaf Forests",          
                   "Tropical Subtropical Coniferous Broadleaf Forests","Temperate Broadleaf Mixed Forests",                   
                   "Temperate Coniferous Forests","Boreal Forests Taiga",                                
                   "Tropical Subtropical Grasslands, Savannas, Shrubland", "Temperate Grasslands, Savannas, Shrubland",           
                   "Flooded Grasslands and Savannas","Montane Grasslands Shrublands" ,                      
                   "Tundra","Mediterranean Forests, Woodlands, Scrub" ,            
                   "Desert and Xeric Shrublands","Mangroves")  
  
  biome_m <- matrix(NA,ncol=3,nrow=length(Biome.names))
  biome_m[,1] <- Biome.names
  biome_m[Biome.names=="Tropical Subtropical Moist Broadleaf Forests",2] <- 1
  biome_m[Biome.names=="Tropical Subtropical Dry Broadleaf Forests",2] <- 2
  biome_m[Biome.names=="Tropical Subtropical Coniferous Broadleaf Forests",2] <- 3
  biome_m[Biome.names=="Temperate Broadleaf Mixed Forests",2] <- 4
  biome_m[Biome.names=="Temperate Coniferous Forests",2] <- 5
  biome_m[Biome.names=="Boreal Forests Taiga",2] <- 6
  biome_m[Biome.names=="Tropical Subtropical Grasslands, Savannas, Shrubland",2] <- 7
  biome_m[Biome.names=="Temperate Grasslands, Savannas, Shrubland",2] <- 8
  biome_m[Biome.names=="Flooded Grasslands and Savannas",2] <- 9
  biome_m[Biome.names=="Montane Grasslands Shrublands",2] <- 10
  biome_m[Biome.names=="Tundra",2] <- 11
  biome_m[Biome.names=="Mediterranean Forests, Woodlands, Scrub",2] <- 12
  biome_m[Biome.names=="Desert and Xeric Shrublands",2] <- 13
  biome_m[Biome.names=="Mangroves",2] <- 14
  
  biome_m[Biome.names=="Tropical Subtropical Moist Broadleaf Forests",3] <- "#ff729c"
  biome_m[Biome.names=="Tropical Subtropical Dry Broadleaf Forests",3] <- "#f95943"
  biome_m[Biome.names=="Tropical Subtropical Coniferous Broadleaf Forests",3] <-"#f57600"
  biome_m[Biome.names=="Temperate Broadleaf Mixed Forests",3] <- "#5aae00"
  biome_m[Biome.names=="Temperate Coniferous Forests",3] <- "#388c62"
  biome_m[Biome.names=="Boreal Forests Taiga",3] <- "#001ef9"#"#3c944d"
  biome_m[Biome.names=="Tropical Subtropical Grasslands, Savannas, Shrubland",3] <- "#fbc100"#"#faa200"
  biome_m[Biome.names=="Temperate Grasslands, Savannas, Shrubland",3] <- "#b7be8d"
  biome_m[Biome.names=="Flooded Grasslands and Savannas",3] <- "#788c82" # not in data
  biome_m[Biome.names=="Montane Grasslands Shrublands",3] <- "#5486ab"
  biome_m[Biome.names=="Tundra",3] <- "#8593f9"
  biome_m[Biome.names=="Mediterranean Forests, Woodlands, Scrub",3] <- "#dacb00"#
  biome_m[Biome.names=="Desert and Xeric Shrublands",3] <- "#f4de00"#"#e8cd00"
  biome_m[Biome.names=="Mangroves",3] <- "#ff8cf7"
  
#  par(mfrow=c(4,4),mar=c(4,4,2,2))
#  for(i in 1:14){
#    plot(rnorm(100),col=biome_m[i,3],pch=16,cex=10,,main=biome_m[i,1])
#   }

  ix <- match(vector_with_cats,biome_m[,2])
  
  return(biome_m[ix,1])
}