plot_Figure_4 <- function(origin){

  # packages
  require(ggrepel)
  require(wordcloud)
  require(tm)
  require(mvtnorm)
  
  require(FactoMineR)
  try(detach(package:calibrate,unload = T))
  try(detach(package:rda,unload = T))
  require(vegan)
  require(textplot)
  
  #---------------------------------------------------------------------------------------
  # load data
  #---------------------------------------------------------------------------------------
  load(file = file.path(origin,"data","master_matrix",paste0("X2.RData")))
  
  info=TRY_Env$info
  trait=TRY_Env$trait
  soil = TRY_Env$soil
  climate = TRY_Env$climate
  latitude = abs(cbind(TRY_Env$info$Lat,TRY_Env$info$max.lat,TRY_Env$info$min.lat))
  
  
  
  color_to_ClimateSoil<- function(vector_now){
    colors_now <-c(soil_col, climate_col)
    out <- rep("white",length(vector_now))
    out[grep(x = vector_now,pattern = "soil")] <- colors_now[1]  
    out[Rename_Vars(vector_now)[,2]=="soil"]<- colors_now[2]  

    out[grep(x = vector_now,pattern = "climate")] <- colors_now[2]  
    out[Rename_Vars(vector_now)[,2]=="climate"]<- colors_now[2]  
    return(out)
  }
  
  # keep only topsoil layer
  soil_c <- soil[,-c(grep(names(soil) ,pattern = "_sl2"),
                   grep(names(soil) ,pattern = "_sl3"),
                   grep(names(soil) ,pattern = "_sl4"),
                   grep(names(soil) ,pattern = "_sl5"),
                   grep(names(soil) ,pattern = "_sl6"),
                   grep(names(soil) ,pattern = "_sl7"),
                   grep(names(soil) ,pattern = "_sd2"),
                   grep(names(soil) ,pattern = "_sd3"),
                   grep(names(soil) ,pattern = "_sd4"),
                   grep(names(soil) ,pattern = "_sd5"),
                   grep(names(soil) ,pattern = "_sd6"),
                   grep(names(soil) ,pattern = "_sd7"))]
  print(paste("Keeping",Rename_Vars(colnames(soil_c))[,3]))

  #--------------------------------------
  # check length of the axes (?)
  #decorana(as.matrix(cbind(soil,climate))+abs(min(as.matrix(cbind(soil,climate)))))
  # all axes below 2 standard deviations -> RDA appropriate
  #decorana(trait)
  # all axes below 2 standard deviations -> RDA appropriate
  
  # scale the predictors
  explanatory_o <- scale(cbind(soil_c,climate))
  explanatory <- explanatory_o# keep _o with unchanged names
  colnames(explanatory) <- Rename_Vars(colnames(explanatory))[,3]
  # log-scale the traits
  response_o <- as.matrix(scale(log(trait)))
  response = response_o# keep _o with unchanged names
  colnames(response) <- Rename_Vars(colnames(response_o))[,3]
  
  e <- data.frame(explanatory) # [,which(colnames(explanatory)%in%ix_imp)]
  r <- data.frame(response)
  dat_rda <- cbind(e,r)
  
  # rda
  m1 <- rda((r+abs(min(r))) ~ ., e)
  m0 <- rda((r+abs(min(r))) ~ 1, e)
    
  m= m1
  par(mfrow=c(1,1))
  # check: ordiplot(m)
  mod=m
  print(RsquareAdj(mod)) # how much can be explained totally
    
  #--- 
  # significance test:
  anova(m)
  sum_eigenvalues = sum(mod$CCA$eig)
  sum(mod$CCA$eig) == mod$CCA$tot.chi
  Var_explained <- as.numeric(mod$CCA$eig/sum(mod$CCA$eig)[[1]])
  barplot(Var_explained*RsquareAdj(mod)$adj.r.squared)
      
  barplot(mod$CCA$eig/sum(mod$CCA$eig))
  constrained_eig <- mod$CCA$eig/mod$CA$tot.chi*100
  unconstrained_eig <- mod$CA$eig/mod$CA$tot.chi*100
    
  summary(mod)
  constrained_eig
  barplot (c(constrained_eig, unconstrained_eig), 
             col = c(rep ('red', length (constrained_eig)), rep ('black', length (unconstrained_eig))), 
             las = 2, ylab = '% variation')

  # fit the ecoregion in the same number of axes
  # fit - > put number of axes
  #rownames(mod$CCA$biplot) <- substr(rownames(mod$CCA$biplot) , start=2,stop=nchar(rownames(mod$CCA$biplot)))
  
  df_now <- cbind(rownames(mod$CCA$biplot),mod$CCA$biplot[,1]*4,mod$CCA$biplot[,2]*4)
  rownames(df_now) <-  1:nrow(df_now)
  colnames(df_now) <- c("names","one","two")  
  df_now <- as.data.frame(df_now)
  
  try(dev.off())
  if(!dir.exists(file.path(origin,"figures","figure_4"))){
    dir.create(file.path(origin,"figures","figure_4"))}
  
  { 
    # Set plot layout
    pdf(file.path(origin, "figures","figure_4",paste0("figure_4_All_In.pdf")), 
        width = 5, height = 6)
    par(mar=c(5,5,0,0))
    
    layout(matrix(c(1,1,
                    1,1,
                    1,1,
                    2,2), nr=4, byrow=T))
    #   layout.show(2)
    
    #----------------------------------------------------------------------------------
    # plot 1
    #----------------------------------------------------------------------------------
    trait_names <- Rename_Vars(colnames(trait))[,3]
    minx=-1
    maxx=1.1
    miny=minx
    maxy=maxx
    plot(1:10, type="n",xlim=c(minx,maxx),ylim=c(miny,maxy),frame=FALSE,
         xlab="RDA1",
         ylab="RDA2",
         cex.lab=2,
         cex.axis=1.5)
    points(mod$CCA$wa[,1:2]*2,col=add.alpha(color_to_biomes(info$BIOME),.7),pch=16,cex=1.5)
    dat_xy=mod$CCA$v[,1:2]
    dat_xy_text <- dat_xy*1.7
    dat_xy_arrows <- dat_xy*1.5
    rownames(mod$CCA$v)
    dat_xy_text[which(rownames(dat_xy_text)=="Seed.length"),1] <- dat_xy_text[which(rownames(dat_xy)=="Seed.length"),1]+.1
    dat_xy_text[which(rownames(dat_xy_text)=="Seed.length"),2] <- dat_xy_text[which(rownames(dat_xy)=="Seed.length"),2]+.06
    dat_xy_text[which(rownames(dat_xy_text)=="Seed.mass"),2] <- dat_xy_text[which(rownames(dat_xy)=="Seed.mass"),2]-.08
    dat_xy_text[which(rownames(dat_xy_text)=="Dispersal.U.length"),1] <-dat_xy_text[which(rownames(dat_xy)=="Dispersal.U.length"),1]+.37
    dat_xy_text[which(rownames(dat_xy_text)=="Conduit.density"),2] <-dat_xy_text[which(rownames(dat_xy)=="Conduit.density"),2]-0.05
    dat_xy_text[which(rownames(dat_xy_text)=="Conduit.density"),1] <-dat_xy_text[which(rownames(dat_xy)=="Conduit.density"),1]-0.1
    dat_xy_text[which(rownames(dat_xy_text)=="Seeds.per.Reprod.U"),2] <-dat_xy_text[which(rownames(dat_xy)=="Seeds.per.Reprod.U"),2]+0.05
    dat_xy_text[which(rownames(dat_xy_text)=="Seeds.per.Reprod.U"),1] <-dat_xy_text[which(rownames(dat_xy)=="Seeds.per.Reprod.U"),1]-0.25
    
    for(t in 1:17){
      arrows(x0 = 0, y0 = 0, x1 = dat_xy_arrows[t,1], y1 = dat_xy_arrows[t,2],col= "darkgray",length = .1,lwd = 1.2)
    }
    
    for(t in 1:17){
      arrows(x0 = 0, y0 = 0, x1 = dat_xy_arrows[t,1], y1 = dat_xy_arrows[t,2],col= color_to_traits(colnames(trait))[t],length = .1,lwd = 1)
    }
    
    textplot(dat_xy_text[,1],dat_xy_text[,2], 
             col=color_to_traits(colnames(trait)), 
             cex=1.4,
             words = trait_names, new = F)
    
    #----------------------------------------------------------------------------------
    # plot 2
    #----------------------------------------------------------------------------------
    par(mar=c(0,0,0,0))
    BiomN=c(14,1,2,7,3,12)
    plot(1:10, type="n",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",frame=FALSE,col="white")
    legend(x = 0,y = 1, name_to_biomes(BiomN), col = color_to_biomes(BiomN),cex=1.4,pt.cex=2,
           text.col = "black", pch = 16,box.col = "white")
    
    
    
    
    #----------------------------------------------------------------------------------
    # plot 2
    #----------------------------------------------------------------------------------
    par(mar=c(5,5,0,0))
    plot(1:10, type="n",xlim=c(minx,maxx),ylim=c(miny,maxy),frame=FALSE,
         xlab="RDA1",
         ylab="RDA2",
         cex.lab=2,
         cex.axis=1.5)
    points(mod$CCA$wa[,1:2]*2,col=add.alpha(color_to_biomes(info$BIOME),.7),pch=16,cex=1.5)
    
    for(v in 1:nrow(mod$CCA$biplot)){
      arrows(x0 = 0, y0 = 0, x1 = mod$CCA$biplot[v,1], y1 = mod$CCA$biplot[v,2],
             col= add.alpha(color_to_ClimateSoil(Rename_Vars(colnames(explanatory_o))[,2])[v],.5),length = .1,lwd = 1)
    }
    
    dat_xy <- mod$CCA$biplot[,1:2]
    dat_xy[rownames(dat_xy)=="Vapour.pressure_min",] <-  dat_xy[rownames(dat_xy)=="Vapour.pressure_min",]+c(-.16,-.01)
    dat_xy[dat_xy<0] <- dat_xy[dat_xy<0]-.03
    dat_xy[dat_xy>0] <- dat_xy[dat_xy>0]+.03
    textplot(dat_xy[,1],dat_xy[,2], 
             col= color_to_ClimateSoil(Rename_Vars(colnames(explanatory_o))[,2]),
             cex=1.2,lwd=1,words= rownames(mod$CCA$biplot),
             new = FALSE)
    
    
    #----------------------------------------------------------------------------------
    # plot 3
    #----------------------------------------------------------------------------------
    par(mar=c(0,0,0,0))
    BiomN=c(13,5,4,8,6,10,11)
    plot(1:10, type="n",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",frame=FALSE,col="white")
    legend(x = 0,y = 1, name_to_biomes(BiomN), col = color_to_biomes(BiomN),cex=1.4,pt.cex=2,
           text.col = "black", pch = 16,box.col = "white")
    
    dev.off()
    
  }   
  
  
  { 
    # Set plot layout
    pdf(file.path(origin, "figures","figure_4",paste0("figure_4_All_In.pdf")), 
        width = 10, height = 6)
   par(mar=c(5,5,0,0))

   layout(matrix(c(1,1,2,2,
                   1,1,2,2,
                   1,1,2,2,
                   3,3,3,3), nr=4, byrow=T))
 #   layout.show(3)

    #----------------------------------------------------------------------------------
    # plot 1
    #----------------------------------------------------------------------------------
    trait_names <- Rename_Vars(colnames(trait))[,3]
    minx=-1
    maxx=1.1
    miny=minx
    maxy=maxx
    plot(1:10, type="n",xlim=c(minx,maxx),ylim=c(miny,maxy),frame=FALSE,
         xlab="RDA1",
         ylab="RDA2",
         cex.lab=2,
         cex.axis=1.5)
    points(mod$CCA$wa[,1:2]*2,col=add.alpha(color_to_biomes(info$BIOME),.7),pch=16,cex=1.5)
    dat_xy=mod$CCA$v[,1:2]
    dat_xy_text <- dat_xy*1.7
    dat_xy_arrows <- dat_xy*1.5
    rownames(mod$CCA$v)
    dat_xy_text[which(rownames(dat_xy_text)=="Seed.length"),1] <- dat_xy_text[which(rownames(dat_xy)=="Seed.length"),1]+.1
    dat_xy_text[which(rownames(dat_xy_text)=="Seed.length"),2] <- dat_xy_text[which(rownames(dat_xy)=="Seed.length"),2]+.06
    dat_xy_text[which(rownames(dat_xy_text)=="Seed.mass"),2] <- dat_xy_text[which(rownames(dat_xy)=="Seed.mass"),2]-.08
    dat_xy_text[which(rownames(dat_xy_text)=="Dispersal.U.length"),1] <-dat_xy_text[which(rownames(dat_xy)=="Dispersal.U.length"),1]+.37
    dat_xy_text[which(rownames(dat_xy_text)=="Conduit.density"),2] <-dat_xy_text[which(rownames(dat_xy)=="Conduit.density"),2]-0.05
    dat_xy_text[which(rownames(dat_xy_text)=="Conduit.density"),1] <-dat_xy_text[which(rownames(dat_xy)=="Conduit.density"),1]-0.1
    dat_xy_text[which(rownames(dat_xy_text)=="Seeds.per.Reprod.U"),2] <-dat_xy_text[which(rownames(dat_xy)=="Seeds.per.Reprod.U"),2]+0.05
    dat_xy_text[which(rownames(dat_xy_text)=="Seeds.per.Reprod.U"),1] <-dat_xy_text[which(rownames(dat_xy)=="Seeds.per.Reprod.U"),1]-0.25
    
    for(t in 1:17){
      arrows(x0 = 0, y0 = 0, x1 = dat_xy_arrows[t,1], y1 = dat_xy_arrows[t,2],col= "darkgray",length = .1,lwd = 1.2)
    }
    
    for(t in 1:17){
      arrows(x0 = 0, y0 = 0, x1 = dat_xy_arrows[t,1], y1 = dat_xy_arrows[t,2],col= color_to_traits(colnames(trait))[t],length = .1,lwd = 1)
    }
    
    textplot(dat_xy_text[,1],dat_xy_text[,2], 
             col=color_to_traits(colnames(trait)), 
             cex=1.4,
             words = trait_names, new = F)
 
    #----------------------------------------------------------------------------------
    # plot 2
    #----------------------------------------------------------------------------------
    par(mar=c(5,5,0,0))
    plot(1:10, type="n",xlim=c(minx,maxx),ylim=c(miny,maxy),frame=FALSE,
         xlab="RDA1",
         ylab="RDA2",
         cex.lab=2,
         cex.axis=1.5)
    points(mod$CCA$wa[,1:2]*2,col=add.alpha(color_to_biomes(info$BIOME),.7),pch=16,cex=1.5)
    
    for(v in 1:nrow(mod$CCA$biplot)){
      arrows(x0 = 0, y0 = 0, x1 = mod$CCA$biplot[v,1], y1 = mod$CCA$biplot[v,2],
             col= add.alpha(color_to_ClimateSoil(Rename_Vars(colnames(explanatory_o))[,2])[v],.5),length = .1,lwd = 1)
    }
    
    dat_xy <- mod$CCA$biplot[,1:2]
    dat_xy[rownames(dat_xy)=="Vapour.pressure_min",] <-  dat_xy[rownames(dat_xy)=="Vapour.pressure_min",]+c(-.16,-.01)
    dat_xy[dat_xy<0] <- dat_xy[dat_xy<0]-.03
    dat_xy[dat_xy>0] <- dat_xy[dat_xy>0]+.03
    textplot(dat_xy[,1],dat_xy[,2], 
             col= color_to_ClimateSoil(Rename_Vars(colnames(explanatory_o))[,2]),
             cex=1.2,lwd=1,words= rownames(mod$CCA$biplot),
             new = FALSE)
  
    
    #----------------------------------------------------------------------------------
    # plot 3
    #----------------------------------------------------------------------------------
    par(mar=c(0,0,0,0))
    BiomN=c(14,1,2,7,3,12)
    plot(1:10, type="n",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",frame=FALSE,col="white")
    legend(x = 0,y = 1, name_to_biomes(BiomN), col = color_to_biomes(BiomN),cex=1.4,pt.cex=2,
           text.col = "black", pch = 16,box.col = "white")
    BiomN=c(13,5,4,8,6,10,11)
    legend(x = .55,y = 1, name_to_biomes(BiomN), col = color_to_biomes(BiomN),cex=1.4,pt.cex=2,
           text.col = "black", pch = 16,box.col = "white")
    
    dev.off()

  }   
  

  

} 


