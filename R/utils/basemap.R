
proj=32198 # projection for distance

r<- readRDS("data/polygons/reference_raster.RDS")

#canadian waters
can<- read_sf("C:/LEHOUX/Projects/Github/zoo_transbound/data/polygons/canada_atlantic_polygon_areas.shp")
can<- can %>%  st_transform(proj)

makeLong <- function(x) paste0(-x, "\u00b0O") # \u00b0 is the Unicode degree symbol

nec<- ne_countries(continent = "North America", scale="large")
basetheme <- theme_few() + theme(axis.title= element_text(colour="black", size=13),
                                axis.text = element_text(color="black", size=11),
                                strip.text =element_text(color="black", size=13),
                                legend.justification = "left")

basemap <-  ggplot() +
  geom_sf(data = nec, fill="bisque2", col="bisque4") +
  coord_sf(xlim=c(-80,-46), ylim=c(35,60)) + #scale_y_continuous(breaks=c(46,47,48,49))+
  basetheme + xlab("Longitude")+ylab("Latitude")


bathy<- read_ncdf("data/glorys/static/GLO-MFC_001_030_mask_bathy.nc", var="deptho") %>%  
  filter(longitude > -80, longitude < -40, latitude >40, latitude <60)
bathy[is.na(bathy)] <- -1000
bathy[bathy >0] <- NA


for(model_res in c(10,30,60)){
  r<- readRDS("data/polygons/reference_raster.RDS")
  r_newres= r
  res(r_newres) <- model_res *1000
bathy<-  st_warp(bathy, crs=proj, cellsize=model_res *1000)#transformation and resample
bathy.r<- as(bathy, "Raster")
bathy.r<- resample(bathy.r, r_newres)
names(bathy.r) <- "depth"
bathy.r[!is.na(bathy.r)] <- 1
necw<- world2raster(raster2world(bathy.r)) %>%
  st_as_stars()  %>% st_as_sf() %>% dplyr::select(-layer) %>%  st_union()
write_rds(necw, paste0("data/polygons/netlogo_basemap",model_res,"_km.RDS"))
rm(model_res)
}
