
seascape_y<- function(start_date, end_date, time_step, model_res=10){
  r<- readRDS("data/polygons/reference_raster.RDS")
  r_newres= r
  res(r_newres) <- model_res *1000
  
dates<- as.character(seq(ymd(start_date), ymd(end_date), 1))
###Daily layers####
source("R/utils/Copernicus_to_stack.R")

#temperature.r <- copernicus_to_stack(data.dir="data/glorys/daily/Glorys12V1_10m_%s.nc", data.name="thetao",
#                                   r=r, can=can, 
#                                   start_date=start_date , 
 #                                  end_date=end_date)
#temperature.r[] <- temperature.r +273
#names(temperature.r) <-  paste("temperature", dates)

sst.r <- copernicus_to_stack(data.dir="data/sat/daily/GLO-SST-L4_%s.nc", data.name="analysed_sst",
                           r=r, can=can, 
                           start_date=start_date, 
                           end_date=end_date)
names(sst.r) <-  gsub(names(sst.r),pattern="layer", replacement="sst")
sst.r <- resample(sst.r, r_newres)


bathy.r<- bathymetry(data.dir="data/glorys/static/GLO-MFC_001_030_mask_bathy.nc", 
                     data.name="deptho", 
                     r=r_newres, can=can)
bathy.r <- resample(bathy.r, r_newres)

source("R/utils/sun.R")
day.r<- sun_len(r=r, dates=dates)
names(day.r) <-  paste("daylength", dates)
day.r<- as(day.r, "Raster")
day.r <- resample(day.r, r_newres)

nafo<- st_read("data/polygons/NAFO/NAFO_SubUnits_CanAtlantic_WGS84_4326.shp")
nafo<- nafo%>% mutate(ID=1:nrow(nafo))
nafo<- st_make_valid(nafo) %>%  dplyr::select(UnitArea, ID) %>%  st_transform(crs=proj)
nafor1 <- terra::rasterize(nafo, rast(r_newres), "UnitArea")
nafor2 <- terra::rasterize(nafo, rast(r_newres), "ID")

key.nafo<- bind_cols(as.points(nafor1) %>%  st_as_sf() %>%  st_drop_geometry,
                     as.points(nafor2) %>%  st_as_sf())%>%  rename(NAFO=ID) %>%  
  mutate(UnitArea=substring(UnitArea, 1,2)) %>%  dplyr::select(-geometry) %>%  distinct()
write_rds(key.nafo, paste0("data/polygons/",model_res,"km_keynafo.RDS"))

nafo.r<- as(nafor2, "Raster")

###Convert zooplancton to daily time steps (repeating)#########
source("R/utils/zooplancton.R")
zoo_layers<- zooplancton(zoo.dir= "C:/LEHOUX/Projects/Github/zoo_transbound/predictions/prediction_bioenergy/glorys/Predictions 2023 RiouxE/Bioenergy_3D/", 
                         r=r,
                         my.year= year(dates[[1]]))

month_inds <- month(dates)  # month numbers 1-12
# Efficient stacking:
zoo.r <- stack(zoo_layers[month_inds]) 
names(zoo.r) <-  paste("zooplancton", dates)
zoo.r <- resample(zoo.r, r_newres)

############Merge to time step##########
n=length(dates)
# Option 1: Fixed 7-day bins from the start date
group <- rep(seq_len(ceiling(n/7)), each=time_step)[1:n]  # Layer -> week index
#or set dates and use isoweek.
# (Keep using Option 1 if you want blocks of 7 layers starting with the first date)
### Step 2: Apply mean by group using terra's tapp() or stackApply()

#temperature.step.r<- stackApply(temperature.r, group, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/stacktemperature_", year(start_date), ".grd"), overwrite=T) 
sst.step.r<- stackApply(sst.r, group, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/stacksst_",model_res, year(start_date), ".grd"), overwrite=T)  
day.step.r<- stackApply(day.r, group, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/stackday_",model_res, year(start_date), ".grd"), overwrite=T)  
zoo.step.r <- stackApply(zoo.r, group, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/stackzoo_",model_res, year(start_date), ".grd"), overwrite=T) 

#no grouping only done to assign file 
group.d =1:n
#temperature.day.r<- stackApply(temperature.r, group.d, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/dstacktemperature_", year(start_date), ".grd"), overwrite=T) 
sst.day.r<- stackApply(sst.r, group.d, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/dstacksst_",model_res, year(start_date), ".grd"), overwrite=T)  
day.day.r<- stackApply(day.r, group.d, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/dstackday_",model_res, year(start_date), ".grd"), overwrite=T) 
zoo.day.r <- stackApply(zoo.r, group.d, mean, na.rm=TRUE, filename= paste0("data/tmp_stack/dstackzoo_",model_res, year(start_date), ".grd"), overwrite=T) 

seascape.step <- list(
 # temperature = temperature.step.r,
  sst         = sst.step.r,
  daylength   = day.step.r,
  zooplankton = zoo.step.r,
  bathy      = bathy.r,    # (bathymetry and nafo have one layer),
  nafo = nafo.r
)

seascape.d <- list(
 # temperature = temperature.day.r,
  sst         = sst.day.r,
  daylength   = day.day.r,
  zooplankton = zoo.day.r,
  bathy      = bathy.r ,    # (bathymetry and nafo have one layer),
  nafo = nafo.r 
)

#ces lignes sont pour éviter de mettre des poissons hors de la zone d'étude et créer des NA dans le prédictions de longueur
seascape.d$bathy[seascape.d$bathy <1000] <- NA
bi<- st_as_sf(rasterToPolygons(seascape.d$bathy)) %>%  st_union(by_feature = F)
zi <-  st_as_sf(rasterToPolygons(seascape.d$zooplankton)) %>%  st_union(by_feature = F)
ss <-  st_as_sf(rasterToPolygons(seascape.d$sst)) %>%  st_union(by_feature = F)
seascape_mask<- st_snap(zi,st_snap(ss,bi, tolerance = 0 ), tolerance = 0 )
write_rds(seascape_mask, file=paste0("data/polygons/",model_res,"km_seascape_mask.RDS"))

write_rds(seascape.d, paste0("data/seascape/seascape_",model_res,"km_daily_", my.year, ".RDS"))
write_rds(seascape.step, paste0("data/seascape/seascape_",model_res,"km_",time_step,"_days_", my.year, ".RDS"))


}


