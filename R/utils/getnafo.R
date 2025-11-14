getnafo <-  function(r.res){
nafo<- st_read("data/polygons/NAFO/NAFO_SubUnits_CanAtlantic_WGS84_4326.shp")
nafo<- nafo%>% mutate(ID=1:nrow(nafo))
nafo<- st_make_valid(nafo) %>%  dplyr::select(UnitArea, ID) %>%  st_transform(crs=proj)
nafor1 <- terra::rasterize(nafo, rast(r.res), "UnitArea")
nafor2 <- terra::rasterize(nafo, rast(r.res), "ID")

nafo.r<- as(nafor2, "Raster")
return(nafo.r)
}