# This figure requires loading of ESRI files, which are not provided in this folder.
Figure_extended_data <- function(origin){
  
  
  #install.packages("sf")
#  library("sf")
#  library(spdep)
#  library(raster)
#  library(sp)
#  install.packages("SpatialEpi")
#  library(SpatialEpi)
#  library(rgdal)
#  library(ncdf4)
   
  #-----------------------------
  # read spatial data
  #-----------------------------
  land.polygon <- readOGR(file.path(origin,"data","helper_files","spatial_data","shp_global110/"), "110m_land")
  land.polygon <- readOGR(file.path(origin,"data","helper_files","spatial_data","shp_global110/"), "110m_land")
  lake.polygon <- readOGR(file.path(origin,"data","helper_files","spatial_data","shp_global110/"), "110m_lakes")
  admin.polygon <- readOGR(file.path(origin,"data","helper_files","spatial_data","shp_global110/"), "110m_admin_0_countries")
  # read spatial script
  source(file.path(origin,"scripts","_support_functions","spatial_data","project_raster.R"))
  
  # -------------------------------------------------------------
  # load data X2
  # -------------------------------------------------------------
  load(file = file.path(origin,"data/master_matrix","X2.RData"))
  load(file = file.path(origin,"data/helper_files/Ecoregions_selected/Ecoregion_Agg2.RData"))
  info <- TRY_Env$info
  info_2 <- info[info$ECO_ID%in%Ecoregion_Agg2$ECO_ID,]
  
  # get the Ecoregions of interest:
  selected_ER <- as.data.frame(cbind(info_2$ECO_ID,info_2$species.count))
  colnames(selected_ER) <- c("ID","nb_traits")
  transform(selected_ER,ID=as.numeric(ID),nb_traits=as.numeric(nb_traits))
  rm("TRY_Env")
  
  # -------------------------------------------------------------
  #  load data X0
  # -------------------------------------------------------------
  load(file = file.path(origin,"data/master_matrix","X0.RData"))
  # cut to selected ERs only
  TRY_Env_o <- TRY_Env[TRY_Env$ECO_ID%in%info_2$ECO_ID,]
  
  # -------------------------------------------------------------
  # get latlon information of observations:
  # -------------------------------------------------------------
  coord1 <- cbind(TRY_Env_o$Lat,TRY_Env_o$Lon)
  rm("TRY_Env")
  rm("TRY_Env_o")
  coord <- unique(coord1)# select only unique coordinates

  #-----------------------------
  # load shapefile
  #-----------------------------
  fileNow = "wwf_terr_ecos.shp"
  fileNpath = file.path(origin,"data","helper_files","EcoregionsShapefiles","wwf_terr_ecos.shp")
  TER <- shapefile(fileNpath)
  proj4string(TER)
  
  #-----------------------------
  # select relevant ecroegions only
  #-----------------------------
  TER_subset = TER[which(TER$ECO_ID %in% selected_ER$ID),]
  
  #-----------------------------
  # Project rasters:
  #-----------------------------
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
  
  #-----------------------------
  # load coordinates of observations
  #-----------------------------
#  list.files(file.path(origin,"data","master_matrix"))
#  load(file.path(origin,"data","master_matrix","X0.RData"))
#  TRY_Env0_o <- TRY_Env
  
#  # cut to relevant Ecoregions:
#  load(file.path(origin,"data","helper_files","Ecoregions_selected","Ecoregion_Agg2.RData"))
#  TRY_Env1=TRY_Env1[TRY_Env1$Group.2%in%Ecoregion_Agg2$ECO_NAME,]
  
#  TRY_Env0 <- TRY_Env0_o[TRY_Env_o$ECO_NAME%in%Ecoregion_Agg2$ECO_NAME,]
  
  {
    
    pdf(file=file.path(origin,"figures","figure_extended_data","Ecoregion_map.pdf"))
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
    #}
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
    
    dev.off()
  }
  
  
}



