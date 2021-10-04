plot_figure_S3_Riverplot_template <- function(origin){
  
  require(riverplot)
  output_term=""
  
  load(file.path(origin, "data", "_results","RidgeRegression","hp_RR",paste0("HP_",nruns=50,"nruns_","Data_GapFilled", "",".RData")))
  hp_now <- hp_l[[which(names(hp_l)%in%climOsoil)]]
  names(hp_now)
  for(i in 1:17){
    for(j in 1:length(hp_now)){
    hp_now[[1]][,i]=abs(rnorm(50,mean=.5,sd=.25))
    hp_now[[2]][,i]=abs(rnorm(50,mean=.5,sd=.25))
    hp_now[[3]][,i]=abs(rnorm(50,mean=.5,sd=.25))
    }}
  
  
  X1=hp_now[[1]]
  X2=hp_now[[2]]
  
  edg=1
  
  colnames(hp_l[[1]][[1]])[1] ="Trait name"
  nm=c(paste0(substring(text = climOsoil,first = 1,last = regexpr("A", climOsoil)[1]-1)),
       paste0(sub(".*A", "", climOsoil)))
  
  RPE_l <- list() 
  trait_now=1
  
  trait_name_now = colnames(hp_l[[1]][[1]])[trait_now]
    
    
  edg=1
  
  hp_up=hp_now[[3]][,1:2]
  hp_low1=hp_now[[1]][,1:2]
  hp_low2 = hp_now[[2]][,1:2]
  arms=4
  #nm=c(paste0("climate_",round(r2_1*100,digits=1)),paste0("soil_",round(r2_2*100,digits=1)))
  nm=c("Climate","Soil")
  # calc pure independent effect for SUBclasses climate and soil
  indep_effects_subclimate = colMeans(hp_low1)
  indep_effects_subsoil = colMeans(hp_low2)
  
  # calc pure independent effect for classes climate and soil
  indep_effect_climate = mean(hp_up[,1])
  indep_effect_soil    = mean(hp_up[,2])
  
  print(trait_name_now)
  print(indep_effects_subsoil)
  print(indep_effects_subclimate)
  
  # scale subgroups to climate (or soils') absolute pure independent effect
  fac_climate = indep_effect_climate / sum(indep_effects_subclimate)
  fac_soil    = indep_effect_soil   /  sum(indep_effects_subsoil)
  indep_effect_climateb  <- indep_effects_subclimate * fac_climate
  indep_effects_subsoilb <- indep_effects_subsoil    * fac_soil  
  # check if correct
  barplot(cbind(sum(indep_effect_climateb) - indep_effect_climate),ylim=c(-.1,.1))
  barplot(cbind(sum(indep_effects_subsoilb)- indep_effect_soil),ylim=c(-.1,.1))
  
  indep_effects_recalc = c( indep_effect_climateb,indep_effects_subsoilb)
  
  dat_1 <- as.data.frame(cbind(indep_effects_recalc,
                         c("I.E. Water (scaled)","I.E. Energy (scaled)","I.E. Soil Chemistry (scaled))","I.E. Soil Physics (scaled)"),
                         c(rep("I.E. Climate",arms/2),rep("I.E. Soil",arms/2)),
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
  

  V_cl_colour <- list()
  l=1
  for(l in 1:length(unique(dat_1$Cat3))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat3)[l]))
  #  V_cl_colour[[new_list_name]] <- list(col = color_to_traits(new_list_name), textcol = "black")
    V_cl_colour[[new_list_name]] <- list(col = color_to_traits(new_list_name), textcol = "black")
    if(new_list_name=="Trait name"){color_now=other_col}#"#bb82ba"}#
  }
  for(l in 1:length(unique(dat_1$Cat2))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat2)[l]))
    if(new_list_name=="I.E. Climate"){color_now=climate_S4}#
    if(new_list_name=="I.E. Soil"){color_now=soil_S4}#
#    if(new_list_name==nm[2]){color_now="#bb82ba"}
#    if(new_list_name==nm[1]){color_now="#96ceed"}
    if(new_list_name=="Trait name"){color_now=other_col}#"#bb82ba"}#
    V_cl_colour[[new_list_name]] <- list(col = color_now, textcol = "black")
  }  
  for(l in 1:length(unique(dat_1$Cat1))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat1)[l]))
    new_list_entry <- list(col = color_now, textcol = "black")
    if(new_list_name=="I.E. Energy (scaled)"){color_now=climtype1_S4}#"ffe474"
    if(new_list_name=="I.E. Water (scaled)"){color_now=climtype2_S4}#
    if(new_list_name=="I.E. Soil Physics (scaled)"){color_now=soiltype2_S4}#
    if(new_list_name=="I.E. Soil Chemistry (scaled)"){color_now=soiltype1_S4}#
    
    if(new_list_name=="I.E. Climate"){color_now=climate_S4}#"#96ceed"}#
    if(new_list_name=="I.E. Soil"){color_now=soil_S4}#"#bb82ba"}#
    if(new_list_name=="Trait name"){color_now=other_col}#"#bb82ba"}#
    V_cl_colour[[new_list_name]] <- list(col = color_now, textcol = "black")
  }  
  V_cl_colour$`Trait name`$col=other_col
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
  
  png(file = file.path(origin,"figures","figure_S3",paste0("Template_River.png")),height=1500,width=4700)
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

