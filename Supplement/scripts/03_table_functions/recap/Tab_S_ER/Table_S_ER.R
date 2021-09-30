
tab_S_ER <- funtion(origin,trait,info){
  library(xtable)
  trait_names <- names(trait)
  
  Table_S4 <- cbind(paste0(info$ECO_NAME,"/ ",info$ECO_ID),
                    paste0(info$species.count," (",info$observation.count.tot,")"),
                    round(info$Kier_richness,digits = 0),
                    paste0(round(info$Lat,digits = 1)," (",round(info$min.lat,digits = 0),";",round(info$max.lat,digits = 0),")"),
                    paste0(round(info$Lon,digits = 1)," (",round(info$min.lon,digits = 0),";",round(info$max.lon,digits = 0),")"),
                    round(info$Kier_area,digits = 0))
  
  colnames(Table_S4) <- c("Name of the ecoregion / ID",
                          "Number of species (and observations)",
                          "Plant Richness",
                          "Lat (min; max)",
                          "Lon (min; max)",
                          "Area")
  
  info$species.count[order(info$species.count,decreasing = T)]
  Table_S4 <- Table_S4[order(info$species.count,decreasing = T),]
  head(Table_S4)

  if(!file.exists(file.path(origin, "tables"))){
    dir.create(file.path(origin, "tables"))}
  if(!file.exists(file.path(origin, "tables","Table_S_ER"))){
    dir.create(file.path(origin, "tables","Table_S_ER"))}
  
  
  library(xtable)
  
  write.csv(Table_S4, file= file.path(origin,"tables","Table_S_ER","Tab_S_ER.csv"))
  tableSb <- xtable(Table_S4, caption="A Very Long Table", label="ALongTable")
  print(tableSb, include.rownames=FALSE, tabular.environment="longtable", floating=FALSE)
  print(xtable(Table_S4, type = "latex"), file =file.path(origin,"tables","Table_S_ER","Tab_S_ER.tex"))

}
