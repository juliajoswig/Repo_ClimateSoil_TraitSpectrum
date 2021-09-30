# Sebastian Sippel
# 05.03.2015


# Function to get projected Raster:
get.projected.raster <- function(raster.object, proj.4) {
  
  # Input 1: project the raster itself:
  raster.object.projected = projectRaster(from=raster.object, to=projectExtent(object=raster.object, crs=proj.4))
  
  # INPUT 2: transform land lines:
  # land.lines = as(land.polygon, "SpatialLines")
  # land.projected <- spTransform(crop(land.lines, y=extent(raster.object)), proj.4)
  
  # INPUT 3: get projected extent:
  polygon.projected = get.projected.polygon(raster.object = raster.object, proj.4=proj.4)
  
  # INPUT 4: make reprojected grid:
  # grid.projected = spTransform(x=gridlines(raster.object), CRSobj=proj.4)
  
  # cut the raster with the polygon
  cells.in.poly <- cellFromPolygon(raster.object.projected, polygon.projected)  # get the IDs of the raster cells inside the polygon
  cells.all <- 1:ncell(raster.object.projected)  # get all cell IDs of the raster
  cells.out.poly <- cells.all[-unlist(cells.in.poly)]  # exclude from the vector with all cell IDs the cells inside the polygon -> get the cell IDs which are outside the polygon
  raster.object.projected[cells.out.poly] <- NA  # set all cells outside the polygon to NA
  
  # return projected raster:
  return(raster.object.projected)
}

# Function to get projected Polygon:
# raster.object = Europe.Tmin.warming.GEV.100.ret.level
# proj.4 = europe_lambert_projection

get.projected.polygon <- function(raster.object, proj.4) {
  # get extent object from test:
  raster.coord = coordinates(raster.object)
  
  # grid cells at the edge:
  edge.cells.left = which(min(raster.coord[,1]) == raster.coord[,1]) # left side
  edge.cells.bottom = which(min(raster.coord[,2]) == raster.coord[,2]) # bottom
  edge.cells.right = which(max(raster.coord[,1]) == raster.coord[,1]) # right side
  edge.cells.top = which(max(raster.coord[,2]) == raster.coord[,2]) # top
  
  
  border.europe = Polygon(coords = raster.coord[c(edge.cells.left, edge.cells.bottom, rev(edge.cells.right), rev(edge.cells.top) ),])
  europe.polygon <- SpatialPolygons(list(Polygons(list(Polygon(border.europe)), 1)), proj4string=CRS(proj4string(raster.object)))  # create a polgon from this matrix
  europe.polygon.lambert = spTransform(x = europe.polygon, CRSobj=proj.4)
  return(europe.polygon.lambert)  
}

# Function to get projected world map:
get.projected.world.map <- function(raster.object, proj.4, land.polygon = land) {
    
  # INPUT 2: transform land lines:
  land.lines = as(land.polygon, "SpatialLines")
  land.projected <- spTransform(crop(land.lines, y=extent(raster.object)), proj.4)
  
  # return projected raster:
  return(land.projected)
}

# Function to get projected grid lines:
get.projected.grid <- function(raster.object, proj.4) {
    
  # INPUT 4: make reprojected grid:
  grid.projected = spTransform(x=gridlines(raster.object), CRSobj=proj.4)
  
  # return projected grid:
  return(grid.projected)
}


# get projected labels:
plot.projected.labels <- function(raster.object, proj.4) {
  
  # INPUT: make labels for reprojected grid:
  test.labels = spTransform(gridat(gridlines(raster.object)), CRSobj=proj.4)
  no.plot.idx = which(test.labels$pos == 2)[1] # nicht plotten!!
  
  text(coordinates(test.labels)[-no.plot.idx,], labels=parse(text=test.labels$labels)[-no.plot.idx],
       pos=test.labels$pos[-no.plot.idx], offset=test.labels$offset[-no.plot.idx])
  return()
}
  

# plot projected raster:
plot.projected.raster <- function(raster.object, proj.4, land.polygon, ...) {
  plot(get.projected.raster(raster.object=raster.object, proj.4=proj.4), axes=F, box=FALSE, ...)
  plot(get.projected.world.map(raster.object=raster.object, proj.4=proj.4, land.polygon=land.polygon), add=TRUE)
  plot(get.projected.polygon(raster.object=raster.object, proj.4=proj.4), add=TRUE)
  plot(get.projected.grid(raster.object=raster.object, proj.4=proj.4), add=T, col="lightgrey")
  plot.projected.labels(raster.object=raster.object, proj.4 = proj.4)
}
  

# --------------------------------------------
## Define new plot projected raster function:
# --------------------------------------------
plot.projected.raster <- function(raster.object, proj.4, land.polygon, ...) {
  
  # Define and plot test raster to get margin:
  test.raster = raster(x=extent(c(extent(raster.object)[1:2], extent(raster.object)[3] - 3, extent(raster.object)[4])))
  values(test.raster) <- 0
  projection(test.raster) <- longlat
  plot( get.projected.raster(raster.object = test.raster, proj.4=proj.4), axes = F, box=F, col="white", legend=F)
  
  plot(get.projected.raster(raster.object=raster.object, proj.4=proj.4), axes=F, box=FALSE, add = T)
  plot(get.projected.world.map(raster.object=raster.object, proj.4=proj.4, land.polygon=land.polygon), add=TRUE)
  plot(get.projected.polygon(raster.object=raster.object, proj.4=proj.4), add=TRUE)
  plot(get.projected.grid(raster.object=raster.object, proj.4=proj.4), add=T, col="darkgrey")
  legend("topleft", "", bty='n')
  plot.projected.labels(raster.object=raster.object, proj.4 = proj.4)
}


plot.projected.raster.no.legend <- function(raster.object, proj.4, land.polygon, original.projection = longlat, 
                                  zlim, col, breaks, use.raster = F, horizontal = T, interpolate = F) {
  projection(raster.object) <- original.projection
  # Define and plot test raster to get margin:
  test.raster = raster(x=extent(c(extent(raster.object)[1:2], extent(raster.object)[3] - 3, extent(raster.object)[4])))
  values(test.raster) <- 0
  projection(test.raster) <- longlat
  plot( get.projected.raster(raster.object = test.raster, proj.4=proj.4), axes = F, box=F, col="white", legend=F, horizontal = horizontal, interpolate = interpolate)
  
  plot(get.projected.raster(raster.object=raster.object, proj.4=proj.4), zlim=zlim, breaks = breaks, axes=F, box=FALSE, col=col, useRaster = use.raster, interpolate=F, legend = F, horizontal = T, add=T)
  plot(get.projected.world.map(raster.object=raster.object, proj.4=proj.4, land.polygon=land.polygon), add=TRUE)
  plot(get.projected.polygon(raster.object=raster.object, proj.4=proj.4), add=TRUE)
  plot(get.projected.grid(raster.object=raster.object, proj.4=proj.4), add=T, col="darkgrey")
  plot.projected.labels(raster.object=raster.object, proj.4 = proj.4)
}

# For horizontal legend
# write legend label:
# par(new=T, mfrow=c(1,1), oma=c(2,0,0,0), mar=c(0,0,0,0))
# plot.new()
# mtext(text=label.text, side=1, line= 1, cex = 1)

# For vertical legend:
# legend("topleft", label.text, bty='n')



