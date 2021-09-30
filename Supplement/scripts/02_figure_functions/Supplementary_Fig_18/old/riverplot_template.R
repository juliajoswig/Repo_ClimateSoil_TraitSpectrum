pl_riverplot_singleTrait_Milan.R

riverplot_it <- function(origin,nruns,atmObio,Agg_type,sel_now,test_sel,yi_now){
  
  require(riverplot)
  
  #   data input 
  input <- as.data.frame(cbind(c(rep(1,10)),
                               rep("heat",10),
                               sample(c("forest","grassland","both"),10,replace = TRUE),
                               sample(c("compensation","partial","no_comp"),10,replace = TRUE)))
  names(input) <- c("Value","Cat1","Cat2","Cat3")
  input <- transform(input, Value = as.numeric(Value))
  data.df4 = input
  
  
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
  data2a = aggregate(x=data.df4[,which(!(names(data.df4) 
                                         %in% c("Cat1","Cat2","Cat3")))],
                     by = list(data.df4$Cat1,data.df4$Cat2), 
                     FUN = new.sum.fun)
  colnames(data2a) <- c("Cat1","Cat2","Value")
  
  
  # aggregate to sum of relative importances per species
  data2b = aggregate(x=data.df4[,which(!(names(data.df4) 
                                         %in% c("Cat1","Cat2","Cat3")))],
                     by = list(data.df4$Cat2,data.df4$Cat3), 
                     FUN = new.sum.fun)
  
  colnames(data2b) <- c("Cat2","Cat3","Value")
  
  
  #------------------------------------------------------------------------------------------------
  # attribute
  #------------------------------------------------------------------------------------------------
  
  ID1   = as.vector(unique(data2a$Cat1))
  ID2   = as.vector(unique(c(as.vector(unlist(data2a$Cat2)),as.vector(unlist(data2b$Cat2)))))
  ID3 =  as.vector(unique(data2b$Cat3))
  
  
  nodes  = data.frame(
    ID = c(ID1,ID2, ID3), 
    x  = c(array(1, length(ID1)),array(2, length(ID2)), array(3, length(ID3))),
    labels = c(ID1, ID2, ID3), 
    cl = c(ID1,ID2, ID3))
  
  
  
  
  # define edge matrix
  edges1 = as.data.frame(list(N1 = data2a$Cat1, 
                              N2 = data2a$Cat2, 
                              Value = data2a$Value))
  
  edges2 = as.data.frame(list(N1 = data2b$Cat2, 
                              N2 = data2b$Cat3, 
                              Value = data2b$Value))
  
  
  
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
  
  
  for(l in 1:length(unique(data.df4$Cat1))){
    
    new_list_name <- as.vector(unlist(unique(data.df4$Cat1)[l]))
    if(new_list_name=="heat"){color_now="red"}else{color_now="gray"}
    new_list_entry <- list(col = color_now, textcol = "black")
    #new_list_entry <- list(col = color_to_SoilAtm(data.df4$soilOatm[which(data.df4$data.type%in%new_list_name)[1]]), textcol = "black")
    V_cl_colour$new_list_entry <- new_list_entry
    names(V_cl_colour)[length(V_cl_colour)] <- new_list_name
    
  }
  for(l in 1:length(unique(data.df4$Cat2))){
    
    new_list_name <- as.vector(unlist(unique(data.df4$Cat2)[l]))
    if(new_list_name=="forest"){color_now="darkgreen"}
    if(new_list_name=="grassland"){color_now="green"}
    if(new_list_name=="both"){color_now="yellow"}
    new_list_entry <- list(col = color_now, textcol = "black")
    #new_list_entry <- list(col = color_to_SoilAtm(data.df4$soilOatm[which(data.df4$data.type%in%new_list_name)[1]]), textcol = "black")
    V_cl_colour$new_list_entry <- new_list_entry
    names(V_cl_colour)[length(V_cl_colour)] <- new_list_name
    
  }  
  for(l in 1:length(unique(data.df4$Cat3))){
    
    new_list_name <- as.vector(unlist(unique(data.df4$Cat3)[l]))
    if(new_list_name=="partial"){color_now="turquoise"}
    if(new_list_name=="no_comp"){color_now="lightblue"}
    if(new_list_name=="compensation"){color_now="blue"}
    new_list_entry <- list(col = color_now, textcol = "black")
    #new_list_entry <- list(col = color_to_SoilAtm(data.df4$soilOatm[which(data.df4$data.type%in%new_list_name)[1]]), textcol = "black")
    V_cl_colour$new_list_entry <- new_list_entry
    names(V_cl_colour)[length(V_cl_colour)] <- new_list_name
    
  }  
  
  
  names(V_cl_colour)
  
  
  
  for (i in 1:length(V_node_def)) {
    V_node_def[[as.character(nodes[i, "ID"])]] = V_cl_colour[[as.character(nodes[i, "cl"])]]
  }
  
  
  #Convert node positions back to Integers
  RPE$nodes$x <- as.numeric(RPE$nodes$x)
  
  RPE$styles = V_node_def
  
  #pdf(file = file.path(origin,"plots","river.pdf"))
  par(mfrow=c(1,1),mar=c(1,1,1,1))
  plot(1:1,frame.plot = FALSE,col="white")
  op = par(cex = 1,no.readonly = TRUE)
  riverplot(RPE, 
            gravity = 'center', 
            nodewidth = 1, 
            default_style = style,
            plot_area = .5)
  
  #dev.off()
  
}