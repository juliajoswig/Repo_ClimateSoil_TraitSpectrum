Figure_extended_data <- function(origin){

  
  #install.packages("sf")
  #  library("sf")
  #  library(spdep)
  library(raster)
  library(sp)
  #install.packages("SpatialEpi")
  #library(SpatialEpi)
  library(rgdal)
  library(ncdf4)
   
  #------------------------------------------------------------------------
  # load data Agg 0
  #------------------------------------------------------------------------
  source(file.path(origin,"scripts","_support_functions","spatial_data","project_raster.R"))
  
  list.files(file.path(origin,"data/master_matrix","_aggregated_agg0","TRY","NA_mnNbr"))
  if(output_term==""){
    load(file = file.path(origin,"data/master_matrix","_aggregated_agg0","TRY","NA_mnNbr","TRY_Env_20191111.RData"))}
  #if(output_term%in%c("woody","non_woody")){
  #  load(file = file.path(origin,"data/master_matrix","_aggregated_agg2","NA_mnNbr","TRY_Env2_KierNONwoody_20200214.RData"))}
  # cut to relevant ecoregions only
  TRY_Env_o <- TRY_Env[TRY_Env$ECO_ID%in%info_2$ECO_ID,]
  sum(TRY_Env_o$ECO_ID%in%info_2$ECO_ID)
  nrow(TRY_Env)
  nrow(TRY_Env_o)
  Appr_type_now="Data_GapFilled"
  #prep data:
  out <- prep_TRY_Env(Appr_type_now,output_term="obs_scale",Rename_Vars,TRY_Env_o)
  info_obs=out$info_2
  rm("TRY_Env")
  
  coord1 <- cbind(info_obs$Lat,info_obs$Lon)
  head(coord1)
  coord1 <- round(coord1,digits = 0)
  coord <- unique(coord1)
  col_points <- NA
  col_points1 <- data.frame(table(paste0(coord1[,1],"_",coord1[,2])))
  vals <- col_points1$Freq
  #Some sample data
  x <- log(vals)
  dat <- data.frame(x = x,y = x^2 + 1)
  
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c("white","yellow","red"))
  
  #This adds a column of color values
  # based on the y values
  col_points <- rbPal(800)[as.numeric(cut(log(dat$y),breaks = 800))]
  plot(dat$x,dat$y,pch = 20,cex=1.5,col = "black")
  points(dat$x,dat$y,pch = 20,col = col_points)
  
  #------------------------------------------------------------------------
  # Packages
  library(raster)
  library(sp)
  library(SpatialEpi)
  library(rgdal)
  library(ncdf4)
  
  #-----------------------------
  # load selected ecoregion
  #-----------------------------
  # get the Ecoregions of interest:
  #  load(file = file.path(origin,"infos_2.RData"))
  if(output_term==""){selected_ER <- as.data.frame(cbind(info_2$ECO_ID,info_2$species.count))}
  if(output_term=="woody"|output_term=="non_woody"){selected_ER <- as.data.frame(cbind(info_2$ECO_ID,rep(20,nrow(info_2))))}
  colnames(selected_ER) <- c("ID","nb_traits")
  transform(selected_ER,ID=as.numeric(ID),nb_traits=as.numeric(nb_traits))
  
  #-----------------------------
  # load shapefile
  #-----------------------------
  fileNow = "wwf_terr_ecos.shp"
  fileNpath = file.path(origin,"data","master_matrix","_orig","Ecoregions","wwf_terr_ecos.shp")
  TER <- shapefile(fileNpath)
  proj4string(TER)
  
  #-----------------------------
  # select relevant ecroegions only
  #-----------------------------
  TER_subset = TER[which(TER$ECO_ID %in% selected_ER$ID),]
  
  # read stuff
  #  setwd("/Net/Groups/BGI/people/ssippel/code/spatial_data/")
  list.files()
  land.polygon <- readOGR(file.path(origin,"data","master_matrix","_orig","spatial_data","shp_global110/"), "110m_land")
  land.polygon <- readOGR(file.path(origin,"data","master_matrix","_orig","spatial_data","shp_global110/"), "110m_land")
  lake.polygon <- readOGR(file.path(origin,"data","master_matrix","_orig","spatial_data","shp_global110/"), "110m_lakes")
  admin.polygon <- readOGR(file.path(origin,"data","master_matrix","_orig","spatial_data","shp_global110/"), "110m_admin_0_countries")
  
  
  # Project rasters:
  test.raster = raster(extent(c(-180,180, -90, 90)), res=2.5)
  longlat="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  projection(test.raster) <- longlat
  values(test.raster) <- 30
  # brks <- quantile(selected_ER[,2], seq(0, 1, 1/8))
  # values(test.raster)[1:10] <- c(brks, 31)
  
  seq(0, 1200, length.out = 9)
  brks = seq(0, 300, length.out = 7)
  library(RColorBrewer)
  library(fields)
  cols <- brewer.pal(n=7, name="YlGn")
  
  {
    # pdf(file=file.path(origin,"figures","Fig_S1","Ecoregion_map.pdf"), width=10, height=7)
    #
    if(output_term==""){pdf(file=file.path(origin,"figures","figure_extended_data","Ecoregion_map.pdf"))}
    if(output_term=="woody"|output_term=="non_woody"){pdf(file=file.path(origin,"figures","figure_S5",paste0("Ecoregion_map_",output_term,".pdf")))}
    
    par(mar=c(0,0,0,0))
    
    arg <- list(at=brks, labels=c("0","50","100", "150", "200", "250", ">300"))
    plot(get.projected.raster(raster.object = test.raster, proj.4=world_robinson_projection), axes = F, col=cols, breaks = brks, box=F, 
         legend=T, horizontal = T, axis.args = arg)
    plot(get.projected.raster(raster.object = test.raster, proj.4=world_robinson_projection), axes = F, col="white", box=F, add=T, legend=F)
    plot(get.projected.polygon(raster.object=test.raster, proj.4=world_robinson_projection), add=TRUE)
    plot(spTransform(x=land.polygon, CRSobj=world_robinson_projection), add=T)
    #plot(spTransform(x=admin.polygon, CRSobj=world_robinson_projection), add=T, col="yellow")
    plot(spTransform(gridlines(admin.polygon), CRSobj=world_robinson_projection), col="brown", add=T, lty=3)
    
    #test = cbind(TER_subset@data, sapply(TER_subset$ECO_ID, FUN=function(x) out$er.NB3$SLA_info[which(c(out$er.NB3$Eco_ID)==x)]))
    # if(output_term==""){
    test = cbind(TER_subset@data, sapply(TER_subset$ECO_ID, FUN=function(x) info_2$species.count[which(info_2$ECO_ID==x)]))
    str(test)
    
    
    #str(test)
    
    #if(output_term=="woody"|output_term=="non_woody"){
    #  test = cbind(TER_subset@data, rep(20,nrow(TER_subset@data)))
    #}
    
    names(test) <- c(names(TER_subset@data), "Data_Density")
    TER_subset@data <- test
    
    #  TER_subset@data <- test
    # brks <- quantile(TER_subset$Data_Density, seq(0, 1, 1/8))
    
    TER_subset_projected = spTransform(x=TER_subset, CRSobj=world_robinson_projection)
    plot(TER_subset_projected, col = cols[findInterval(TER_subset$Data_Density, brks, all.inside=TRUE)], add=T)
    
    plot.projected.labels(raster.object = test.raster, proj.4 = world_robinson_projection)
    test.sp = SpatialPoints(coords = coord[,2:1])
    proj4string(test.sp) = longlat
    points(spTransform(test.sp, CRSobj = world_robinson_projection),pch=15,col="black",cex=.4)
    points(spTransform(test.sp, CRSobj = world_robinson_projection),pch=15,col=col_points,cex=.25)
    
    dev.off()
  }
  
  {
    # pdf(file=file.path(origin,"figures","Fig_S1","Ecoregion_map.pdf"), width=10, height=7)
    #
    if(output_term==""){pdf(file=file.path(origin,"figures","figure_extended_data","Points_only.pdf"))}
    if(output_term=="woody"|output_term=="non_woody"){pdf(file=file.path(origin,"figures","figure_S5",paste0("Ecoregion_map_",output_term,".pdf")))}
    
    par(mar=c(0,0,0,0))
    
    arg <- list(at=brks, labels=c("0","50","100", "150", "200", "250", ">300"))
    plot(get.projected.raster(raster.object = test.raster, proj.4=world_robinson_projection), axes = F, col=cols, breaks = brks, box=F, 
         legend=T, horizontal = T, axis.args = arg)
    plot(get.projected.raster(raster.object = test.raster, proj.4=world_robinson_projection), axes = F, col="white", box=F, add=T, legend=F)
    plot(get.projected.polygon(raster.object=test.raster, proj.4=world_robinson_projection), add=TRUE)
    plot(spTransform(x=land.polygon, CRSobj=world_robinson_projection), add=T)
    #plot(spTransform(x=admin.polygon, CRSobj=world_robinson_projection), add=T, col="yellow")
    plot(spTransform(gridlines(admin.polygon), CRSobj=world_robinson_projection), col="brown", add=T, lty=3)
    
    #test = cbind(TER_subset@data, sapply(TER_subset$ECO_ID, FUN=function(x) out$er.NB3$SLA_info[which(c(out$er.NB3$Eco_ID)==x)]))
    # if(output_term==""){
    test = cbind(TER_subset@data, sapply(TER_subset$ECO_ID, FUN=function(x) info_2$species.count[which(info_2$ECO_ID==x)]))
    str(test)
    
    
    #str(test)
    
    #if(output_term=="woody"|output_term=="non_woody"){
    #  test = cbind(TER_subset@data, rep(20,nrow(TER_subset@data)))
    #}
    
    names(test) <- c(names(TER_subset@data), "Data_Density")
    TER_subset@data <- test
    
    #  TER_subset@data <- test
    # brks <- quantile(TER_subset$Data_Density, seq(0, 1, 1/8))
    
    TER_subset_projected = spTransform(x=TER_subset, CRSobj=world_robinson_projection)
    #plot(TER_subset_projected, col = cols[findInterval(TER_subset$Data_Density, brks, all.inside=TRUE)], add=T)
    
    #plot.projected.labels(raster.object = test.raster, proj.4 = world_robinson_projection)
    test.sp = SpatialPoints(coords = coord[,2:1])
    proj4string(test.sp) = longlat
    points(spTransform(test.sp, CRSobj = world_robinson_projection),pch=15,col="black",cex=.4)
    points(spTransform(test.sp, CRSobj = world_robinson_projection),pch=15,col=col_points,cex=.25)
    
    dev.off()
  }
  
}

