create_all_figures <- function(origin, version_now,Appr_type_now,
                               whichfig){
  
  if(whichfig%in%"4"){}
  if(whichfig%in%"S1"){}
  if(whichfig%in%"S2"){}
  if(whichfig%in%"S3"){}
  
  
  if(whichfig%in%"1a"){
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_1","pl_figure_1a.R"))
    plot_Figure_1a(origin, output_term="")}
  }
  
  if(whichfig%in%"1b"){
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_1","pl_figure_1b.R"))
    plot_Figure_1b(origin, output_term="")
  }

  if(whichfig%in%"2"){
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_2","pl_figure_2_2020.R"))
    plot_Figure_2(origin, output_term="")
  }
  
  if(whichfig%in%"3a"){
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3_overlaySqunb.R"))
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3a.R"))
    plot_figure_3a(origin,whichfig = "3a")
  }
  if(whichfig%in%"3b"){
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3b.R"))
    plot_figure_3b(origin,whichfig = "3a")
  }



  if(whichfig%in%"S4"){
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3_overlaySqunb.R"))
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3a.R"))
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_S4","pl_figure_S4_Riverplot.R"))
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_S4","pl_figure_S4_Riverplot.R"))
    plot_figure_3a(origin,whichfig = "S4")
    plot_figure_3b(origin,whichfig = "S4")# some things to be resolved...
    plot_figure_S4_barplot(origin)
    plot_figure_S4_Riverplot(origin)
  }
  
  if(whichfig%in%"S5"){  
      source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_1","pl_figure_1a.R"))
      source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_1","pl_figure_1b.R"))
      for(output_term in c("woody","non_woody")){
      plot_Figure_1a(origin,output_term)
      plot_Figure_1b(origin, output_term)
    } 
  }

  if(whichfig%in%"S6"){  
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_2","pl_figure_2_2020.R"))
    for(output_term in c("woody","non_woody")){
      plot_Figure_2(origin, output_term)
    } 
  }

  if(whichfig%in%"S7"){  
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3_overlaySqunb.R"))
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3a.R"))
      plot_figure_3a(origin,whichfig=="S7")
    source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3b.R"))
      plot_figure_3b(origin,whichfig=="S7")
  }

  source(file.path(origin,"scripts",version_now,"02_figure_functions","fig_3","pl_figure_3a.R"))
  Figure_3a(origin,version_now,output_term,
            hp_now=hp_l[[counter]], sel_now)
  
  source(file.path(origin,"scripts",version_now,"02_figure_functions","fig_3","plot_Figure_3b.R"))
  hp_now = hp_l[[1]]
  Figure_3b(hp_now,origin,sel_now,output_term)
  hp_now = hp_l[[2]]
  Figure_3b(hp_now,origin,sel_now,output_term)
  
  
  par(mfrow=c(1,1))
  barplot(colMeans(r2_l$soilAclimate$r2_soilAclimate),ylim=c(0,1),las=3)
  #,col=color_to_traits(names(trait)))
  
  barplot(rbind(colMeans(hp_l[[counter]][[1]]-hp_l[[counter]][[3]]),
                colMeans(hp_l[[counter]][[3]]),
                colMeans(hp_l[[counter]][[2]]-hp_l[[counter]][[3]])),
          col=c("red","lightgray","blue"),pch=16,main=sel_now,las=3)
  
  list.files(file.path(origin,"scripts",version_now,"02_figure_functions","fig_3" ))
  
  #Figure_S3(origin,Appr_type_now)    
  #source(file.path(origin,"scripts",version_now ,"02_figure_functions","fig_3","pl_figure_3_20200115.R"))
  #Figure_3(origin,Appr_type_now) 
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------
  # Tables
  Table_S4(origin)
  #----------------------------------------------------------------------------------------
  # plot 
  #----------------------------------------------------------------------------------------
  # plot RDA (fig4):
  plot_Figure_4(...)
  # plot PCA (fig1):
  # not yet implemented
  # plot lat gradient of soil and climate (figR1):
  
  
  
  # read & write out ("/Results/tables/...") data for: 
  Table_1(origin)
  Table_S5(origin)
  # if(output_term==""){  
  #   load(file.path(origin, "data", "_results","RidgeRegression","hp_RR",paste0("HP_",nruns,"nruns_","Data_GapFilled", output_term,".RData")))
  #   hp_RR <- hp_l
  #   load(file.path(origin, "data", "_results","RidgeRegression","hp_PLS_pcaTRUE",paste0("HP_",nruns,"nruns_","Data_GapFilled", output_term,".RData")))
  #   hp_PLSt <- hp_l
  #   load(file.path(origin, "data", "_results","RidgeRegression","hp_PLS_pcaFALSE",paste0("HP_",nruns,"nruns_","Data_GapFilled", output_term,".RData")))
  #   hp_PLSf <- hp_l
  #   load(file.path(origin, "data", "_results","RidgeRegression","hp_RF",paste0("HP_",nruns,"nruns_","Data_GapFilled", output_term,".RData")))
  #   hp_RF <- hp_l
  #   print("loaded.")
  # }
}