zooread <- function(my.year, zoo.dir) {
  # Use more efficient file listing and pattern matching
  lfiles <- list.files(
    path = zoo.dir,
    pattern = paste0(".*", my.year, ".*3D\\.RDS$"),
    full.names = TRUE
  )

  # Error handling for no files found
  if (length(lfiles) == 0) {
    stop(paste("No files found for year:", my.year))
  }

  # Read and combine files efficiently
  zoo_data <- data.table::rbindlist(
    lapply(lfiles, readRDS),
    fill = TRUE, # Handle different column structures if needed
    use.names = TRUE
  )

  return(zoo_data)
}



zooplancton <- function(zoo.dir, r, my.year) {
  northb <- st_read("C:/LEHOUX/Projects/Github/zoo_transbound/data/polygons/Northumberland_polygons_to_remove.shp")

  zooy <- zooread(my.year = my.year, zoo.dir = zoo.dir) %>%
    filter(Zlayer < 50) %>%
    mutate(zoomg.tot = DW_Zlayer_mg_cfin_glac + DW_Zlayer_mg_chyp) %>%
    group_by(X, Y, month) %>%
    summarize(zoomg.tot = sum(zoomg.tot)) %>%
    st_as_sf(coords = c("X", "Y"))
  st_crs(zooy) <- 4326

  zoo <- st_transform(zooy, st_crs(proj))
  # zoo is stable for a given month.
  # For each month, create a zoo.terra raster layer (store in a list)
  zoo_layers <- list()
  for (m in 1:12) {
    cat("Rasterizing zoo for month", m, "\n")
    zoo_m <- zoo %>%
      filter(month == m) %>%
      mutate(zoomg.tot = replace_na(zoomg.tot, 0))
    zoo_layers[[m]] <- rasterize(zoo_m, as(r, "Raster"), field = "zoomg.tot", background = 0)
    zoo_layers[[m]] <- mask(zoo_layers[[m]], northb %>% st_transform(crs = proj), inverse = T)
    zoo_layers[[m]] <- mask(zoo_layers[[m]], can)
  }
  return(zoo_layers)
}
