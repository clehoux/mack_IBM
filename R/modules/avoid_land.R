
find_next_position <- function(lwho, 
                               fish,           
                               time_step,      
                               radius_path,    
                               seamap,       
                               tseamap,
                               model_res) {
  path0 <- radius_path %>% filter(who == lwho)
    if (nrow(path0) == 0) return(NULL)
  max.dist <- length_to_distance(
    turtle = turtle(fish, who = lwho),
    param = paramInit(time_step = time_step),
    grid.resolution=model_res
  ) * time_step 
  
  origin_df <- inspect(fish, who = path0$who) %>% as.data.frame()
  # This checks columns exist AND at least one row
  if (!all(c("xcor", "ycor") %in% colnames(origin_df)) || nrow(origin_df) == 0) return(NULL)
  
  goal_df <- path0 %>% as.data.frame()
  if (!all(c("pxcor", "pycor") %in% colnames(goal_df)) || nrow(goal_df) == 0) return(NULL)
  
  origin <- st_as_sf(origin_df, coords = c("xcor", "ycor"))
  goal <- st_as_sf(goal_df, coords = c("pxcor", "pycor"))
  
  distance <- st_distance(origin, goal)
  origin <- bind_cols(origin %>% st_coordinates(), distance = distance[, 1]) %>% as.matrix()
  
  origin.int <- raster::extract(seamap, origin[, 1:2])
  goal.int <- raster::extract(seamap, goal)
  goal <- cbind(goal.int, goal) %>% filter(!is.na(goal.int), !is.na(foodGo))
  goal <- goal %>% st_coordinates()
  
  if (nrow(goal) == 0 | any(is.na(origin))) {
    return(NULL)
  }
  
  AtoB <- tryCatch({
    shortest <- st_as_sf(shortestPath(tseamap, origin = origin[1:2], goal = goal, output = "SpatialLines"))
    shortest %>% mutate(L1 = 1:nrow(goal))
  },
  error = function(e) {
    return(NULL)
  }
  )
  
  
  
 if(!is.null(AtoB)){
   next_pos <- st_cast(bind_rows(AtoB), "MULTIPOINT") %>%
    st_coordinates() %>%
    as.data.frame() %>%
    group_by(L1) %>%
    mutate(n=row_number()) %>%
    filter(n < max.dist) %>%
    slice_tail(n=1) %>%
    ungroup() %>%
    mutate(pxcor = X, pycor = Y, who = lwho, to_keep = 1) %>%
    dplyr::select(pxcor,pycor,  who, to_keep)} else(next_pos=NULL)
  
  next_pos
}


avoid_land <-  function(fish,           
                        time_step, 
                        seaworld, 
                        radius,
                        prob_moving,
                        model_res){
#for paths: find smallest distance avoiding land
seamap=world2raster(seaworld)[[1]]
#this turns all landmass to missing
seamap[seamap==0] <- NA


#assign unit cost to all grid cells in water
seamap[seamap!=-999] <- 1
tseamap <- transition(seamap, mean, directions = 8)
tseamap <- geoCorrection(tseamap, "r")
radius_path <- radius %>% group_by(who) %>% 
  filter( prob > prob_moving & breed %in% c("adult", "juvenile")) %>%
  arrange(desc(foodGo),desc(tempGo), by_group=T) %>% 
  dplyr::select(pxcor, pycor, who, tempGo, foodGo)

all_who <- unique(radius_path$who)


to_keep_all <- map_df(
  all_who, 
  function(x) find_next_position(
    lwho = x,
    fish = fish,
    time_step = time_step,
    radius_path = radius_path,
    seamap = seamap,
    tseamap = tseamap,
    model_res=model_res
  )
 )

radius_to_keep<- left_join(radius, to_keep_all) %>%  filter(to_keep==1)

return(radius_to_keep)
}