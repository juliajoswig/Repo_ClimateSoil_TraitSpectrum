plot_multipanel_PCA_Riverplot <- function(origin){
  
  #install.packages("riverplot")
  require(riverplot)
  output_term="PCA"
  climOsoil="soilAclimate"
  
  load(file.path(origin, "data", "_results","RidgeRegression","_RidgeRegression_results",paste0("HP_",50,"nruns_", output_term,"doPCA",".RData")))

  
  
  if(!dir.exists(file.path(origin,"figures","figure_multipanel_PCA"))){
    dir.create(file.path(origin,"figures","figure_multipanel_PCA"))}
  
  if(!dir.exists(file.path(origin,"figures","figure_multipanel_PCA","all"))){
    dir.create(file.path(origin,"figures","figure_multipanel_PCA","all"))}
  
  hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]
  
  X1=hp_now[[1]]
  X2=hp_now[[2]]
  
  edg=1
  
  nm=c(paste0(substring(text = climOsoil,first = 1,last = regexpr("A", climOsoil)[1]-1)),
       paste0(sub(".*A", "", climOsoil)))
  
  RPE_l <- list() 
  trait_now=1
  
  trait_now=3
  for(trait_now in 1:5){
  trait_name_now = colnames(hp_l[[1]][[1]])[trait_now]
  
  names(hp_l)
  hp_up=hp_l[names(hp_l)=="soilAclimate"]
  hp_low1=hp_l[names(hp_l)=="climate_waterAclimate_energy"]
  hp_low2 = hp_l[which(names(hp_l)%in%c("soil_chemistryAsoil_physics"))]
  arms=4
  #nm=c(paste0("climate_",round(r2_1*100,digits=1)),paste0("soil_",round(r2_2*100,digits=1)))
  nm=c("Climate","Soil")
  # calc pure independent effect for SUBclasses climate and soil
  indep_effects_subclimate = c(mean(hp_low1[[1]][[1]][,trait_now]-hp_low1[[1]][[3]][,trait_now]),
                               mean(hp_low1[[1]][[2]][,trait_now]-hp_low1[[1]][[3]][,trait_now]))
  indep_effects_subsoil = c(mean(hp_low2[[1]][[1]][,trait_now]-hp_low2[[1]][[3]][,trait_now]),
                            mean(hp_low2[[1]][[2]][,trait_now]-hp_low2[[1]][[3]][,trait_now]))
  
  # calc pure independent effect for classes climate and soil
  indep_effect_climate = mean(hp_up[[1]]$indep_climate[,trait_now] - hp_up[[1]]$joint_climate[,trait_now])
  indep_effect_soil    = mean(hp_up[[1]]$indep_soil[,trait_now]    - hp_up[[1]]$joint_soil[,trait_now])
  
  print(trait_name_now)
  print(indep_effects_subsoil)
  print(indep_effects_subclimate)
  
  # scale subgroups to climate (or soils') absolute pure independent effect
  fac_climate = indep_effect_climate / sum(indep_effects_subclimate)
  fac_soil    = indep_effect_soil   /  sum(indep_effects_subsoil)
  indep_effect_climateb  <- indep_effects_subclimate * fac_climate
  indep_effects_subsoilb <- indep_effects_subsoil    * fac_soil  
  # check if correct
  #barplot(cbind(sum(indep_effect_climateb) - indep_effect_climate),ylim=c(-.1,.1))
  #barplot(cbind(sum(indep_effects_subsoilb)- indep_effect_soil),ylim=c(-.1,.1))
  
  indep_effects_recalc = c( indep_effect_climateb,indep_effects_subsoilb)
  
  dat_1 <- as.data.frame(cbind(indep_effects_recalc,
                         c("Water","Energy","Soil Chemistry","Soil Physics"),
                         c(rep("Climate",arms/2),rep("Soil",arms/2)),
                         rep(trait_name_now,arms)))
  names(dat_1) <- c("Value","Cat1","Cat2","Cat3")
  dat_1 <- transform(dat_1, Value = as.numeric(as.vector(dat_1$Value)))
  dat_1 <- dat_1[0<(dat_1$Value),]
  
  
  #-----------------------------------------------------------------------------------------------
  # aggregate 
  #------------------------------------------------------------------------------------------------
  
  
  new.sum.fun <- function(x) {
    if (is.numeric(x)) {
      return(sum(x, na.rm=T))
    } else {
      return(unique(x))
    }
  }
  
  
  # aggregate to sum of relative importances per species
  dat_2a = aggregate(x=dat_1[,which(!(names(dat_1) 
                                         %in% c("Cat1","Cat2","Cat3")))],
                     by = list(dat_1$Cat1,dat_1$Cat2), 
                     FUN = new.sum.fun)
  colnames(dat_2a) <- c("Cat1","Cat2","Value")
  dat_2a <- as.data.frame(dat_2a)
  
  # aggregate to sum of relative importances per species
  dat_2b = aggregate(x=dat_1[,which(!(names(dat_1) 
                                         %in% c("Cat1","Cat2","Cat3")))],
                     by = list(dat_1$Cat2,dat_1$Cat3), 
                     FUN = new.sum.fun)
  dat_2b <- as.data.frame(dat_2b)
  colnames(dat_2b) <- c("Cat2","Cat3","Value")
  # correct for actual r2 of trait:
#  dat_2b$Value[1] <- r2_1 #climate value 
#  dat_2b$Value[2] <- r2_2 #climate value 
  
  
  
  #------------------------------------------------------------------------------------------------
  # attribute
  #------------------------------------------------------------------------------------------------
  
  ID1   = as.vector(unique(dat_2a$Cat1))
  ID2   = as.vector(unique(c(as.vector(unlist(dat_2a$Cat2)),as.vector(unlist(dat_2b$Cat2)))))
  ID3 =  as.vector(unique(dat_2b$Cat3))
  
  
  nodes  = data.frame(
    ID = c(ID1,ID2, ID3), 
    x  = c(array(1, length(ID1)),array(2, length(ID2)), array(3, length(ID3))),
    labels = c(ID1, ID2, ID3), 
    cl = c(ID1,ID2, ID3))
  
  
  
  
  # define edge matrix
  edges1 = as.data.frame(list(N1 = dat_2a$Cat1, 
                              N2 = dat_2a$Cat2, 
                              Value = dat_2a$Value))
  
  edges2 = as.data.frame(list(N1 = dat_2b$Cat2, 
                              N2 = dat_2b$Cat3, 
                              Value = dat_2b$Value))
  
  
  
  RPE = makeRiver(nodes = nodes, edges = rbind(edges1,edges2))
  #RPE = makeRiver(nodes = nodes, edges = edges1)

  
  style <- list(nodestyle= "regular",
                srt = 0, 
                lty = 1)
  
  V_node_def = vector(mode = "list", length = length(nodes[, "ID"]))
  names(V_node_def) = nodes[, "ID"]
  trait_col=color_to_traits(colnames(hp_l[[1]][[1]]))

  V_cl_colour <- list()
  l=1
  for(l in 1:length(unique(dat_1$Cat3))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat3)[l]))
  #  V_cl_colour[[new_list_name]] <- list(col = color_to_traits(new_list_name), textcol = "black")
    if(new_list_name=="trait_PCA_1"){V_cl_colour[[new_list_name]] <- list(col = color_to_traits("LeArea"), textcol = "black")}
    if(new_list_name=="trait_PCA_2"){V_cl_colour[[new_list_name]] <- list(col = color_to_traits("LeN"), textcol = "black")}
    if(new_list_name=="trait_PCA_3"){V_cl_colour[[new_list_name]] <- list(col = color_to_traits("VesLen"), textcol = "black")}
    if(new_list_name=="trait_PCA_4"){V_cl_colour[[new_list_name]] <- list(col = color_to_traits("VesLen"), textcol = "black")}
    if(new_list_name=="trait_PCA_5"){V_cl_colour[[new_list_name]] <- list(col = color_to_traits("VesLen"), textcol = "black")}
  }
  for(l in 1:length(unique(dat_1$Cat2))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat2)[l]))
    if(new_list_name=="Climate"){color_now=climate_S4}#
    if(new_list_name=="Soil"){color_now=soil_S4}#
#    if(new_list_name==nm[2]){color_now="#bb82ba"}
#    if(new_list_name==nm[1]){color_now="#96ceed"}
    V_cl_colour[[new_list_name]] <- list(col = color_now, textcol = "black")
  }  
  for(l in 1:length(unique(dat_1$Cat1))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat1)[l]))
    new_list_entry <- list(col = color_now, textcol = "black")
    if(new_list_name=="Energy"){color_now=climtype1_S4}#"ffe474"
    if(new_list_name=="Water"){color_now=climtype2_S4}#
    if(new_list_name=="Soil Physics"){color_now=soiltype2_S4}#
    if(new_list_name=="Soil Chemistry"){color_now=soiltype1_S4}#
    if(new_list_name=="Climate"){color_now=climate_S4}#"#96ceed"}#
    if(new_list_name=="Soil"){color_now=soil_S4}#"#bb82ba"}#
    V_cl_colour[[new_list_name]] <- list(col = color_now, textcol = "black")
  }  
  
  names(V_cl_colour)
  
#  plot(1:19,col="#b4e7e2",cex=4,pch=16)
#  plot(1:19,col=rgb(.77, .177, .244),cex=4,pch=16)
  
  for (i in 1:length(V_node_def)) {
    V_node_def[[as.character(nodes[i, "ID"])]] = V_cl_colour[[as.character(nodes[i, "cl"])]]
  }
  
  
  #Convert node positions back to Integers
  RPE$nodes$x <- as.numeric(RPE$nodes$x)
  

  
  RPE$styles = V_node_def
  RPE_l[[trait_now]] <- RPE
  
  png(file = file.path(origin,"figures","figure_multipanel_PCA","all",paste0(colnames(hp_l[[1]][[1]])[trait_now],"_River.png")),height=1500,width=2000)
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
  save(RPE_l,file=file.path(origin,"figures","figure_multipanel_PCA",paste0("Riverplot_info.RData")))

  # load riverplot
  load(file.path(origin,"figures","figure_multipanel_PCA",paste0("Riverplot_info.RData")))
  
  for(trait_now in 1:17){
  #  png(file = file.path(origin,"figures","figure_multipanel_PCA",paste0("figure_multipanel_PCA_river_",colnames(hp_l[[1]][[1]])[trait_now],".png")),height=1500,width=2000)
    png(file = file.path(origin,"figures","figure_multipanel_PCA","all",paste0(colnames(hp_l[[1]][[1]])[trait_now],"_River.png")),height=1500,width=2000)
    par(mfrow=c(1,1))
    plot(1:15,frame.plot = FALSE,col="white")
    op = par(cex = 4.1,no.readonly = TRUE)
    riverplot(RPE_l[[trait_now]], 
              gravity = 'center', 
              nodewidth = 7, 
              default_style = style,
              plot_area = .95)
    
    dev.off()
  }
  


}

