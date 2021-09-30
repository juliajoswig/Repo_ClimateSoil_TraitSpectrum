
Table_S4 <- funtion(origin){
  setwd("/Net/Groups")
  orig_loctem =  "BGI"#"/Volumes/BGI" 
  #orig_loctem =  "/Volumes/BGI"#"/Volumes/BGI" # "BGI" or "/Volumes/BGI" # or "/Net/bgi/" or "" or ...
  output_term="_sel20_"#_minmaxmed
  spec_count=20 # if output_term == "_sel20_"
  NA_now="NA_mnNbr"#"NA_stays_NA"
  
  # ---------------------------------------------------------------------------------------
  # define the origin path
  origin = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019")
  
  # get some functions
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_Rename_Vars3.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_put_into_traitGroup.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_traits.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","fn_color_to_biomes.R"))
  source(file.path(origin,"scripts","Data_GapFilled","support_scripts","001a_prep_TRY_Env.R"))
  
  # ---------------------------------------------------------------------------------------
  # load data
  load(file = file.path(orig_loctem,"work_1/2018_Dichotomy/V_2/Revision_2019/data/master_matrix",
                        "_aggregated_agg2",NA_now,"TRY_Env2_Kier_20191111.RData"))
  
  TRY_Env_o <- TRY_Env2_Kier
  # ---------------------------------------------------------------------------------------
  # process data:
  out <- prep_TRY_Env("Data_GapFilled",output_term,Rename_Vars,TRY_Env_o,spec_count)
  info_2=out$info_2
  trait_2=out$trait_2

  trait_names <- names(trait_2)
  
  Table_S4 <- cbind(paste0(info_2$ECO_NAME,"/ ",info_2$ECO_ID),
                    paste0(info_2$species.count," (",info_2$observation.count.tot,")"),
                    round(info_2$Kier_richness,digits = 0),
                    paste0(round(info_2$Lat,digits = 1)," (",round(info_2$min.lat,digits = 0),";",round(info_2$max.lat,digits = 0),")"),
                    paste0(round(info_2$Lon,digits = 1)," (",round(info_2$min.lon,digits = 0),";",round(info_2$max.lon,digits = 0),")"),
                    round(info_2$Kier_area,digits = 0))
  
  colnames(Table_S4) <- c("Name of the ecoregion / ID",
                          "Number of species (and observations)",
                          "Plant Richness",
                          "Lat (min; max)",
                          "Lon (min; max)",
                          "Area")
  
  info_2$species.count[order(info_2$species.count,decreasing = T)]
  Table_S4 <- Table_S4[order(info_2$species.count,decreasing = T),]
  head(Table_S4)
#  write.csv(Table_S4,file=file.path(origin,"manuscript","Tables","Table_S4.csv"))
#  write(Table_S4,file=file.path(origin,"manuscript","Tables","Table_S4.csv"))
#  write.table(Table_S4, file=file.path(origin,"manuscript","Tables","Table_S4.txt"), append = FALSE, sep = " ", dec = ".",
#              row.names = TRUE, col.names = TRUE)
  #install.packages("xlsx")
  require("xlsx")
  tail(Table_S4)
  write.xlsx(Table_S4, file=file.path(origin,"data","Results","tables","Tab_S4","Table_S4.xls"), sheetName="Sheet1", 
             col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)
}