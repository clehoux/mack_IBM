 sun_len<- function (r, dates){

# r: your SpatRaster

# Get coords for every cell (in whatever crs(r)), then transform to lon/lat
pts <- as.data.frame(terra::xyFromCell(r, 1:ncell(r)))
colnames(pts) <- c("x", "y")
pts.sf <- st_as_sf(pts, coords=c("x", "y"), crs=crs(r))
pts.sf <- st_transform(pts.sf, 4326)
lat <- st_coordinates(pts.sf)[,2]

doy<- yday(dates)

# Vectorize: For each date, get daylength for the full vector of latitudes
daylen <- outer(lat, doy, function(lat, doy) daylength(lat, doy))

# This makes a matrix [ncell, ndate]: convert to raster stack
daylen_stack <- rast(lapply(seq_along(dates), function(i) {
  r0 <- rast(r)
  values(r0) <- daylen[,i]
  r0
}))

return(daylen_stack)
}