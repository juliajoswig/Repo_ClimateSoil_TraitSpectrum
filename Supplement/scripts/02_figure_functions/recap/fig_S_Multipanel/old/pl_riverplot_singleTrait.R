plot_Figure_n_Appendix_indivTraits <- function(origin,nruns,Agg_type,sel_now){
  
  atmObio="soilAatmosphere"
  meanOsd="median"
    
  accuracy_tot <- matrix(NA,ncol=5,nrow=0)
  colnames(accuracy_tot) <- c("trait","r2","meanOsd","atmObio","subset")
  require('stringr')

  #source("/Users/jjoswig/Documents/PhD/Code/tools/fn_install.packages_offline.R")
  #install.packages_offline("/Users/jjoswig/Documents/PhD/Code/Packages","riverplot_0.5.tar")
  require("riverplot")
  data <- data.frame(matrix(NA,ncol=10,nrow=0))

  #-------------------------------------------------------------
  # load data
  #-------------------------------------------------------------
  
  # load test data
  load(file.path(origin,"data","VarSelect_output",paste0(nruns,"Reps"),paste0(atmObio,"_trait_",meanOsd,"test_",Agg_type,sel_now,".RData")))
  df_final2 <- test$df_final2

  #------------------------------------------------------------------------------------------------
  # rename predictors
  #------------------------------------------------------------------------------------------------
  data.df <- df_final2#df_tot#rbind(data,df_tot)
  trait.names <- unique(gsub(paste0("_",meanOsd),"",data.df$trait))
  trait_type <- as.vector(unlist(data.df$trait))
  t=1
  for(t in 1:length(trait.names)){
  trait_type <- gsub(paste0(trait.names[t],"_"),replacement = "",trait_type)}

  data.df_start <- data.df
  t=1
  for(t in 1:length(trait.names)){
    
     data.df <- data.df_start[data.df_start$trait==trait.names[t],]

  #------------------------------------------------------------------------------------------------
  # rename type of driver
  #------------------------------------------------------------------------------------------------

    driver_type <- rep(NA,nrow(data.df))
    i=1
    
    data_now <- data.df
    data_now$relationship <-  rename_what_can_be_renamed(data_now$relationship)

    data.df2 <- subset(data.df,select = c("rel_Importance","trait","driver","relationship","trait_info","reg.coef"))
    head(data.df2)
    colnames(data.df2) <- c("rel_Importance","trait","driver","data.type","trait_type" ,"reg.coef" )
    data.df2<- transform(data.df2, rel_Importance = as.numeric(as.vector(unlist(rel_Importance))))
    
    #------------------------------------------------------------------------------------------------
    # rename drivers
    #------------------------------------------------------------------------------------------------
    driver_rename <- as.vector(unlist(data.df2$driver))
      driver_rename[driver_rename=="Tair"] <- "Temperature"
      driver_rename[driver_rename=="Qair"] <-"Humidity"
      driver_rename[driver_rename=="Precip"] <- "Precipitation"
      driver_rename[driver_rename=="H"] <-"Sensible heat"
      driver_rename[driver_rename=="SWdown"] <-"Radiation"

     driver_rename[driver_rename=="topsoilPH"] <-"pH"
     driver_rename[driver_rename=="subsoilPH"] <-"pH"
     driver_rename[driver_rename%in%c("topsoilSand","SNDPPT")] <-"Texture"
     driver_rename[driver_rename%in%c("topsoilSilt","SLTPPT")] <-"Texture"
     driver_rename[driver_rename=="GeologicAges"] <-"Age_geol"
     driver_rename[driver_rename=="TopographicOpenness"] <-"Topographic"
     driver_rename[driver_rename=="topographicWetness"] <-"Topographic"
     driver_rename[driver_rename=="topsoilBulkDensity"] <-"BulkDensity"
     driver_rename[driver_rename=="BLDFIE"] <-"BulkDensity"
     driver_rename[driver_rename=="CRFVOL"] <-"Gravel"
     driver_rename[driver_rename=="topsoilGravel"] <-"Gravel"
     driver_rename[driver_rename%in%c("topsoilClay","CLYPPT")] <-"Texture"
     driver_rename[driver_rename%in%c("CECSOL","CECSUM")] <-"CEC"
     driver_rename[driver_rename=="BLDFIE"] <-"BulkDenstiy"
     driver_rename[driver_rename%in%c("ORCDRC","OCSTHA","OCDENS")] <-"orgC"
     driver_rename[driver_rename%in%c("PHIHOX","PHIKCL","topsoilPH","subsoilPH")] <-"pH"
     driver_rename[driver_rename%in%c(paste0("AWCh",1:7),"AWCtS")] <-"SoilWater"
     driver_rename[driver_rename%in%c(paste0("WWP",1:7),"WWP")] <-"SoilWater"
     driver_rename[driver_rename=="topsoilGravel"] <-"Gravel"
     driver_rename[driver_rename=="topsoilClay"] <-"Texture"
    
     data.df2$driverAgg <-  driver_rename

     
     # complete information on driver.
     driver_rename <- as.vector(unlist(data.df2$driver))
       driver_rename[driver_rename=="Tair"] <- "Temp"
       driver_rename[driver_rename=="Qair"] <-"Humid"
       driver_rename[driver_rename=="Precip"] <- "Precip"
       driver_rename[driver_rename=="H"] <-"SHeat"
       driver_rename[driver_rename=="SWdown"] <-"Rad"

       driver_rename[driver_rename=="topsoilPH"] <-"pH_top"
       driver_rename[driver_rename=="subsoilPH"] <-"pH_sub"
       driver_rename[driver_rename%in%c("topsoilSand")] <-"Sand_top"
       driver_rename[driver_rename%in%c("SNDPPT")] <-"Sand"
       driver_rename[driver_rename%in%c("topsoilSilt")] <-"Silt_top"
       driver_rename[driver_rename%in%c("SLTPPT")] <-"Silt"
       driver_rename[driver_rename=="GeologicAges"] <-"Age_geol"
       driver_rename[driver_rename=="TopographicOpenness"] <-"TopoOpenness"
       driver_rename[driver_rename=="topographicWetness"] <-"TopoWetness"
       driver_rename[driver_rename=="topsoilBulkDensity"] <-"BDens_topl"
       driver_rename[driver_rename=="BLDFIE"] <-"BDens"
       driver_rename[driver_rename=="CRFVOL"] <-"Gravel"
       driver_rename[driver_rename=="topsoilGravel"] <-"Gravel"
       driver_rename[driver_rename%in%c("topsoilClay")] <-"Clay_top"
       driver_rename[driver_rename%in%c("CLYPPT")] <-"Clay"
       driver_rename[driver_rename%in%c("CECSOL","CECSUM")] <-"CEC"
       driver_rename[driver_rename%in%c("OCDENS")] <-"orgC density"
       driver_rename[driver_rename%in%c("ORCDRC")] <-"orgC content"
       driver_rename[driver_rename%in%c("OCSTHA")] <-"orgC stock"
       driver_rename[driver_rename%in%c("PHIHOX")] <-"pH H2O"
       driver_rename[driver_rename%in%c("PHIKCL")] <-"pH KCl"
       driver_rename[driver_rename%in%c("topsoilPH")] <-"pH_topsoil"
       driver_rename[driver_rename%in%c("subsoilPH")] <-"pH_subsoil"
       driver_rename[driver_rename%in%c(paste0("AWCh1"))] <-"mPot -10"
       driver_rename[driver_rename%in%c(paste0("AWCh2"))] <-"mPot -20"
       driver_rename[driver_rename%in%c(paste0("AWCh3"))] <-"mPot -30"
       driver_rename[driver_rename%in%c("AWCtS")] <-"SWsaturated"
       driver_rename[driver_rename%in%c(paste0("WWP",1:7),"WWP")] <-"WCapToWilt"
       driver_rename[driver_rename=="topsoilGravel"] <-"Gravel_top"
      
       driver_rename2 <- rename_what_can_be_renamed(data.df2$driver)#driver_rename
       
     data.df2$driver2 <-  driver_rename#rename_for_plot(driver_rename)
     
     #------------------------------------------------------------------------------------------------
     # soil or atm?
     #------------------------------------------------------------------------------------------------
     
     data.df2$soilOatm <- put_into_soilOrAtm(data.df2$driver)
     data.df3 <- data.df2
     head(data.df2)
    
    
    #------------------------------------------------------------------------------------------------
    # kick out some traits 
    #------------------------------------------------------------------------------------------------
    head(data.df3)
    data.df3$trait =="SeedInvest"
    unique(data.df3$trait)
    ### already out....?
    
 
    
    #------------------------------------------------------------------------------------------------
    # aggregate 
    #------------------------------------------------------------------------------------------------
    
    
    new.sum.fun <- function(x) {
      if (is.numeric(x)) {
        return(sum(x, na.rm=T))
      } else {
        return(unique(x))
      }
    }

    data.df4 <- subset(data.df3,select = c("rel_Importance","data.type","trait","driverAgg","driver2","soilOatm","reg.coef"))
    colnames(data.df4) <-                c("rel.importance","data.type","trait","driver","driver2","soilOatm","reg.coef")
    
    data.df4$data.type <- paste0(data.df4$driver2,"_",data.df4$data.type)
    head(data.df4)
    data.df4 <- as.data.frame(data.df4[data.df4$rel.importance!=0,])
    data.df4 <- transform(data.df4, rel.importance = as.numeric(rel.importance))
    data.df4 <- transform(data.df4, reg.coef = as.numeric(as.vector(unlist(reg.coef))))
    
    # aggregate to sum of relative importances per species
    data2a = aggregate(x=data.df4[,which(!(names(data.df4) 
                                 %in% c("data.type","trait","driver","meanOsd","reg.coef")))],
                         by = list(data.df4$driver,data.df4$trait), 
                         FUN = new.sum.fun)
    data2a <- subset(data2a,select = c("Group.1","Group.2","rel.importance"))
    colnames(data2a) <- c("driver","trait","rel.importance")

      
    # aggregate to sum of relative importances per species
    data2b = aggregate(x=data.df4[,which(!(names(data.df4) 
                                           %in% c("data.type","trait","driver","meanOsd","reg.coef")))],
                       by = list(data.df4$data.type,data.df4$driver), 
                       FUN = new.sum.fun)
    
    colnames(data2b) <- c("data.type","driver","rel.importance")
    

  #------------------------------------------------------------------------------------------------
  # attribute
  #------------------------------------------------------------------------------------------------
     
     ID2   = as.vector(unique(data2a$driver))
     ID1   = as.vector(unique(data2b$data.type))
     ID3 =  as.vector(unique(data2a$trait))
   #  ID4 =  as.vector(unique(data2c$direction))


      nodes  = data.frame(
        ID = c(ID1,ID2, ID3), 
        x  = c(array(1, length(ID1)),array(2, length(ID2)), array(3, length(ID3))),
        labels = c(ID1, ID2, ID3), 
        cl = c(ID1,ID2, ID3))
     
  
 
     
     # define edge matrix
     edges1 = as.data.frame(list(N1 = data2b$data.type, 
                                 N2 = data2b$driver, 
                                 Value = data2b$rel.importance))
     
     edges2 = as.data.frame(list(N1 = data2a$driver, 
                                 N2 = data2a$trait, 
                                 Value = data2a$rel.importance))
     
     
     RPE = makeRiver(nodes = nodes, edges = rbind(edges1,edges2))
     #RPE = makeRiver(nodes = nodes, edges = edges1)
     
     
   
  
  
   style <- list(nodestyle= "regular",
                srt = 0, 
                lty = 1)
  
  V_node_def = vector(mode = "list", length = length(nodes[, "ID"]))
  names(V_node_def) = nodes[, "ID"]
  ot_col=rep("wheat",18)
  S_col=rep("lightblue",18)
  L_col=rep("lightgray",18)
  
  V_cl_colour <- list()
  l=1
  
  
   for(l in 1:length(unique(data.df4$data.type))){

     new_list_name <- unique(data.df4$data.type)[l]
     new_list_entry <- list(col = color_to_PlusMinus(data.df4,new_list_name), textcol = "black")
     #new_list_entry <- list(col = color_to_SoilAtm(data.df4$soilOatm[which(data.df4$data.type%in%new_list_name)[1]]), textcol = "black")
     V_cl_colour$new_list_entry <- new_list_entry
     names(V_cl_colour)[length(V_cl_colour)] <- new_list_name

   }
   
  
    for(l in 1:length(unique(data.df4$driver))){
      
      new_list_name <- unique(data.df4$driver)[l]
      new_list_entry <- list(col = color_to_SoilAtm(data.df4$soilOatm[which(data.df4$driver%in%new_list_name)[1]]), textcol = "black")
      V_cl_colour$new_list_entry <- new_list_entry
      names(V_cl_colour)[length(V_cl_colour)] <- new_list_name
      
    }
  
  
  # for the trait:
  new_list_name <- unique(data.df4[,3])
  new_list_entry <- list(col = color_to_traits(new_list_name), textcol = "black")
  V_cl_colour$new_list_entry <- new_list_entry
  names(V_cl_colour)[length(names(V_cl_colour))] <- new_list_name
  
   names(V_cl_colour)
  
  
  
  for (i in 1:length(V_node_def)) {
    V_node_def[[as.character(nodes[i, "ID"])]] = V_cl_colour[[as.character(nodes[i, "cl"])]]
  }
  
  
  #Convert node positions back to Integers
  RPE$nodes$x <- as.numeric(RPE$nodes$x)
  
  RPE$styles = V_node_def
  
   #png(file.path(origin,"plots","Riverplot",paste0("Type",atmObio,"_",meanOsd,"Riverplot_",".png")),height=1000,width=1000)
  png(file.path(origin,"plots","IndividualTraits","Riverplot",paste0("Riverplot_",atmObio,"_",meanOsd,"Riverplot_",trait.names[t],".png")),height=3000,width=2000)
  par(mfrow=c(1,1))
  plot(1:15,frame.plot = FALSE,col="white")
  op = par(cex = 4.1,no.readonly = TRUE)
  riverplot(RPE, 
            gravity = 'center', 
            nodewidth = 7, 
            default_style = style,
            plot_area = .95)
  
  dev.off()
  } 
  #dev.off()
  
  #}
  
}