tr=i
riverplot_S4 <- function(origin,hp_now,tr,N3_1,r2_1,N2_1,N3_2,r2_2,N2_2){
  require(riverplot)
  
  arms=length(c(names(N3_1),names(N3_2)))
  nm=c(paste0("climate_",round(r2_1*100,digits=1)),paste0("soil_",round(r2_2*100,digits=1)))
  
  input <- as.data.frame(cbind(as.vector(c(N3_1,N3_2)),
                               rep(colnames(hp_now$ind_TWO)[tr],arms),
                               c(rep(nm[1],arms/2),rep(nm[2],arms/2)),
                               c(names(N3_1),names(N3_2))))
  names(input) <- c("Value","Cat1","Cat2","Cat3")
  dat_1 <- transform(input, Value = as.numeric(as.vector(Value)))
  dat_1 <- dat_1[0!=(dat_1$Value),]
  
  
  
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
#  dat_2a = aggregate(x=dat_1[,which(!(names(dat_1) 
#                                         %in% c("Cat1","Cat2","Cat3")))],
#                     by = list(dat_1$Cat1,dat_1$Cat2), 
#                     FUN = new.sum.fun)
  dat_2a <- as.data.frame(cbind(rep(colnames(hp_now$ind_TWO)[tr],2),
                               c(nm[1],nm[2]),
                               c(r2_1,r2_2)))
  colnames(dat_2a) <- c("Cat1","Cat2","Value")
  dat_2a <- transform(dat_2a, Value = as.numeric(as.vector(dat_2a$Value)))
  
  
  # aggregate to sum of relative importances per species
  dat_2b = aggregate(x=dat_1[,which(!(names(dat_1) 
                                         %in% c("Cat1","Cat2","Cat3")))],
                     by = list(dat_1$Cat2,dat_1$Cat3), 
                     FUN = new.sum.fun)
  colnames(dat_2b) <- c("Cat2","Cat3","Value")
  
  
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
  trait_col=color_to_traits(rownames(N3_climate))

  V_cl_colour <- list()
  l=1
  for(l in 1:length(unique(dat_1$Cat1))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat1)[l]))
    V_cl_colour[[new_list_name]] <- list(col = color_to_traits(new_list_name), textcol = "black")
  }
  for(l in 1:length(unique(dat_1$Cat2))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat2)[l]))
    if(new_list_name==nm[2]){color_now=add.alpha(color_to_traits("LeN"),alpha = 0.5)}
    if(new_list_name==nm[1]){color_now=add.alpha(color_to_traits("LeArea"),alpha = 0.5)}
    V_cl_colour[[new_list_name]] <- list(col = color_now, textcol = "black")
  }  
  for(l in 1:length(unique(dat_1$Cat3))){
    new_list_name <- as.vector(unlist(unique(dat_1$Cat3)[l]))
    new_list_entry <- list(col = color_now, textcol = "black")
    if(new_list_name=="energy"){color_now="#ffe474"}#"ffe474"
    if(new_list_name=="soilwater"){color_now="#4055a0"}#
    if(new_list_name=="climatewater"){color_now="#4db1f4"}#
    if(new_list_name=="nutrients"){color_now="#a42a27"}#
    if(new_list_name=="soiltopo"){color_now="#d55817"}#
    if(new_list_name=="wind"){color_now="#b4e7e2"}#
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
  
  #RPE$edges <- RPE$edges[c(1,2,order(RPE$edges$N1[3:length(RPE$edges$N1)])),]
  RPE$edges <- RPE$edges[c(1,2,
                           c(order(RPE$edges$N1[3:length(RPE$edges$N1)])+2))
                         ,]
  
  RPE$edges <- RPE$edges[c(1,2,
                           which(RPE$edges$N2=="energy"),
                           which(RPE$edges$N2=="climatewater"),
                           which(RPE$edges$N2=="wind"),
                           which(RPE$edges$N2=="nutrients"),
                           which(RPE$edges$N2=="soiltopo"),
                           which(RPE$edges$N2=="soilwater"))
                           ,]
  RPE$nodes
  #RPE2 <- RPE

  RPE$nodes <- RPE$nodes[c(1,2,3,
                           which(RPE$nodes$ID=="energy"),
                           which(RPE$nodes$ID=="climatewater"),
                           which(RPE$nodes$ID=="wind"),
                           which(RPE$nodes$ID=="nutrients"),
                           which(RPE$nodes$ID=="soiltopo"),
                           which(RPE$nodes$ID=="soilwater"))
                           ,]
  
  
  RPE$styles = V_node_def
  
#  pdf(file = file.path(origin,"plots","Fig_S4x",paste0(rownames(N3_climate)[tr],"_River.pdf")),height=25,width=25)
  png(file = file.path(origin,"plots","Fig_S4x",paste0(rownames(N3_climate)[tr],"_River.png")),height=1500,width=2000)
  print(rownames(N3_climate)[tr])
  par(mfrow=c(1,1))
    plot(1:15,frame.plot = FALSE,col="white")
    op = par(cex = 4.1,no.readonly = TRUE)
    riverplot(RPE, 
              gravity = 'center', 
              nodewidth = 7, 
              default_style = style,
              plot_area = .95)
  
  dev.off()
  
  print(RPE$edges)
#  op = par(cex = 1,no.readonly = TRUE)
#  riverplot(RPE,
#            gravity = 'center', 
#            nodewidth = 7, 
#            default_style = style)

}
