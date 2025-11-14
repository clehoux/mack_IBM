
copernicus_to_stack <- function(data.dir, data.name, r, can=NULL, start_date, end_date){
if(nchar(start_date) > 4){
    dates<- as.character(seq(ymd(start_date), ymd(end_date), 1))
    workers=2
} 

if(nchar(start_date) == 4) {
  dates<- as.character(start_date)#year only
workers=1
}

plan(multisession, workers=workers)  # Adjust number of cores
files <- sprintf(data.dir, dates)
files<- files[file.exists(files)]


process_stack <- function(f){
  n.c <-  read_ncdf(f, proxy=F, var= c(data.name)) %>%  
    drop_units()
  n.r<-  st_warp(n.c, crs=proj, cellsize=10000)#transformation and resample
  n.r[is.na(n.r)] <- 0
  n.r<- as(n.r, "Raster")
  return(n.r)
}

slayers <- future_lapply(files, process_stack, future.seed=TRUE)
sstack <- stack(slayers)
if(!is.null(can)) sstack<- mask(sstack, can)
return(sstack)

}

bathymetry<- function(data.dir="data/glorys/static/GLO-MFC_001_030_mask_bathy.nc", data.name="deptho", r, can=NULL){
  
#bathymetry
bathy<- read_ncdf(data.dir, var=data.name) %>%  
  filter(longitude > -80, longitude < -40, latitude >35, latitude <60)

bathy<-  st_warp(bathy, crs=proj, cellsize=10000)#transformation and resample
bathy[is.na(bathy)] <- 0
bathy.r<- as(bathy, "Raster")
names(bathy.r) <- "depth"

if(!is.null(can)) bathy.r<- mask(bathy.r, can)

return(bathy.r)

}