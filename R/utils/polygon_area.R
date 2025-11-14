##mackerel overwintering area create a polygon from bathymetry
bathy<- read_ncdf("data/glorys/static/GLO-MFC_001_030_mask_bathy.nc", var="deptho") %>%  
  filter(longitude > -80, longitude < -40, latitude >35, latitude <60)

plot(bathy)
overwintering <-  bathy %>%  filter(latitude < 47, latitude > 42, longitude < -55, longitude > -65) %>%  drop_units() %>% mutate(poly= if_else(deptho > 500 & deptho < 2000, 1,NA))
plot(overwintering[2])
p = st_as_sf(overwintering[2], merge = F,as_points=F)
p <-  st_union(p, is_coverage=T)
p<- st_simplify(p, dTolerance = 0.15)

ggplot() + geom_sf(data=p)
basemap + geom_sf(data=p) + coord_sf(xlim=c(-80,-46), ylim=c(35,60))
#+temp > 7 not necessarily sat because maq are at depth.
st_write(p, "data/polygons/overwintering/overwintering_area2.shp")

##mackerel spawning area create a polygon from stations
spawn<- read.delim("../../iml-mackerel/03.0_egg-index/data/lookup_station_egg.txt")

spawnsf<- spawn %>%  dplyr::select(longitude, latitude) %>%  st_as_sf(coords = c("longitude", "latitude"), crs=4326)

spawnpoly<- st_buffer(spawnsf %>%  st_transform(crs=32198), 60000)
spawnpoly <-  st_union(spawnpoly) %>%  st_transform(crs=4326)

basemap + geom_sf(data=spawnpoly) + coord_sf(xlim=c(-80,-46), ylim=c(35,60))
#+temp > 7 not necessarily sat because maq are at depth.
st_write(spawnpoly, "data/polygons/overwintering/spawninggsl_area.shp", append=F)#overwrite

