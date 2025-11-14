####needs to be tested is it good practise to save within the function. 
#maube return results and then save would we more efficient? 
#but seems to be working with data frames??

run_sim_ibm <- function(replicate_id, my.year, model_res, time_step, barrier, d.d., prob_moving, temp.var="sst", zoo.var="zooplankton", param_to_modify = NULL, percent_change = 0) {
  # Load required packages and modules for each worker
  source("R/utils/loadpackages.R")
  source("R/utils/movement.R") #probability of moving based on temperature (depth)
  source("R/utils/parameters.R")
  
  step.name    <- ifelse(time_step ==7, "7_days_", "daily_")
  param=paramInit(time_step, param_to_modify = param_to_modify, 
                  percent_change = percent_change)
  
  
  
  seascape <- readRDS(paste0("data/seascape/seascape_", model_res, "km_", step.name, my.year, ".RDS"))
  fish <- readRDS(paste0("data/seascape/fish_", model_res, "km_", my.year, ".RDS"))
  key.nafo <- readRDS(paste0("data/polygons/", model_res, "km_keynafo.RDS"))
  
  start_date <- ymd(paste0(my.year, "-01-01"))
  end_date <- ymd(paste0(my.year, "-12-30"))

  dates <- seq.Date(start_date, end_date, time_step)
  fishDyn_list <- vector("list", length(dates))
  popSim_list <- vector("list", length(dates))
 
  
  # --- SIMULATION LOOP (your existing code unchanged) ---
  # for (t in seq_along(dates)) {
  #   [Insert your per-time-step simulation code here]
  # }
  # For demonstration, just record t and Nfish:
  for (t in seq_along(dates)) {
    lyr<- length(seascape) # last on is nafo so not included
    
    seascape2 <- c(
      lapply(seascape[-c((lyr-1) : lyr)], function(l) as(l[[t]], "Raster")),
      seascape[(lyr-1) : lyr]
    ) 
    seascape2<- stack(seascape2)
    
    tempCue<- calc(seascape2[[1]],fun=function(x){dnorm(x-273, mean=11, sd=2)/dnorm(11, mean=11, sd=2)})
    
    names(tempCue) <- "tempCue"
    foodCue<- raster::overlay(seascape2[[1:3]], fun=function(x,y,z){
      (y / 12) * exp((-param$Ea / param$boltz) * ((1 / (x)) - (1 / param$Tref))) *
        (z/1000/0.2) / (param$h + (z/1000/0.2) )})
    names(foodCue) <- "foodCue"
    
    seascape2 <- stack(x = seascape2, tempCue, foodCue)
    
    seaworld <- raster2world(seascape2)
    
    #####1. Mortality####  
    # check mortality (Z)
    
    newNprop <- 1 - exp(-(param$M.a + (NLwith(fish, var="breed", val="adult")$`F` * time_step))) # proportion dying
    fish <- NLset(turtles = fish, agents = NLwith(fish, var="breed", val="adult"), var = "N", val = floor(NLwith(fish, var="breed", val="adult")$N - (newNprop * NLwith(fish, var="breed", val="adult")$N)))
    
    newNprop <- 1 - exp(-(param$M.a * (26.8/NLwith(fish, var="breed", val="juvenile")$`length` * time_step))) # proportion dying
    fish <- NLset(turtles = fish, agents = NLwith(fish, var="breed", val="juvenile"), var = "N", val = floor(NLwith(fish, var="breed", val="juvenile")$N - (newNprop * NLwith(fish, var="breed", val="juvenile")$N)))
    
    newNprop.eggs <- 1 - exp(-(param$M.e)) # proportion dying
    fish <- NLset(turtles = fish, agents = NLwith(fish, var="breed", val=c("eggs", "larvae")), var = "N", val = floor(NLwith(fish, var="breed", val=c("eggs", "larvae"))$N - (newNprop.eggs * NLwith(fish, var="breed", val=c("eggs", "larvae"))$N)))
    
    dying<- fish$who[of(agents=fish, var="weight") < of(agents=fish, var="Wstruct") | of(agents=fish, var="N") <1]
    
    if(length(dying)>0)  fish<- die(fish, who= dying)
    #update the number of who  
    Nfish <- length(unique(fish@.Data[, "who"]))
    
    fish<- breed_transformation(fish=fish, time_step=time_step)
    
    radius1 <- inRadius(agents = fish, radius = length_to_distance(fish, param = param, grid.resolution=model_res) * time_step, agents2 = patches(seaworld))
    radiusdd <- inRadius(agents = fish, radius = length_to_distance(fish, param = param, grid.resolution=model_res) * time_step, agents2 = fish) # density dependance
    #towards(fish, radius1[,1:2])
    whos= of(agents=fish, var="who") 
    breeds= of(agents=fish, var="breed") 
    
    neigh <- NetLogoR::neighbors(world = seaworld, agents = fish, nNeighbors = 8)
    id_to_whos<- data.frame(id=unique(neigh[,"id"]), who=whos, breed=breeds)
    
    foodneigh <- of(world = seaworld, agents = neigh, var = "foodCue")
    sstneigh <- of(world = seaworld, agents = neigh, var = "tempCue")
    nafoneigh <- of(world = seaworld, agents = neigh, var = "nafo")
    neighOK <- cbind(merge(neigh,id_to_whos), inside = foodneigh, sst.inside=sstneigh, nafo.inside=nafoneigh) %>%
      as.data.frame() %>%
      filter(!is.na(inside) | inside !=0, !is.na(sst.inside))
    
    pHere <- patchHere(world = seaworld, turtles = fish)
    
    dd_who <- merge(radiusdd, cbind(who = whos, pHere, inspect(turtles = fish, who = whos)[, c("N", "weight")]))
    dd_Here <- dd_who %>%
      as.data.frame() %>%
      group_by(pxcor, pycor) %>%
      mutate(n = n(), biomass = n / Nfish * N * weight)
    
    dd_Here <- left_join(
      patches(seaworld) %>% as.data.frame(),
      dd_Here %>% dplyr::select(pxcor, pycor, biomass) %>% distinct()
    ) %>%
      mutate(biomass = replace_na(biomass, 0))
    
    
    nafoGo <- of(world = seaworld, agents = radius1, var = "nafo")
    nafoHere <- of(world = seaworld,  agents = coordinates(fish), var = "nafo")
    
    tempGo <- of(world = seaworld, agents = radius1, var = "tempCue")
    foodGo <- of(world = seaworld, agents = radius1, var = "foodCue")
    zooGo <- of(world = seaworld, agents = radius1, var = "zooplankton")
    
    tempHere <- of(world = seaworld, agents = coordinates(fish), var = "tempCue")
    foodHere <- of(world = seaworld, agents = coordinates(fish), var = "foodCue")
    
    Here <- list(
      cbind(tempHere, who = whos) %>% as.data.frame(),
      cbind(foodHere, who = whos) %>% as.data.frame(),
      cbind(nafoHere, who = whos) %>% as.data.frame()
    ) %>% reduce(left_join)
    
    # stochasticity if the fish is moving or at random.
    stoch <- data.frame(prob = runif(n = Nfish, min = 0, max = 1), who = whos)
    neighOK <- left_join(neighOK, stoch)
    state_who <- bind_cols(who = whos, state = of(agents = fish, var="state"))
    
    # DECISION TREE
    radius1 <- left_join(radius1 %>% as.data.frame(), dd_Here %>% distinct(), multiple = "first")
    radius <- list(Here, stoch, state_who, cbind(merge(radius1, id_to_whos), foodGo, tempGo, zooGo, nafoGo) %>%
                     as.data.frame()) %>%
      purrr::reduce(left_join) %>%
      ungroup() %>%
      dplyr::group_by(who) %>%
      # mutate(flag = cumsum(is.na(depthGo))) %>%
      # filter(flag == 0, depthGo < 1000) %>%
      ungroup() %>%
      mutate()
    #not a function code is run.
    
    if(barrier) radius_to_keep =avoid_land(fish=fish,           
                                        time_step=time_step, 
                                        seaworld=seaworld, 
                                        radius=radius,
                                        prob_moving=prob_moving,
                                        model_res=model_res)
    if(!barrier) radius_to_keep =radius
    # feeding
    whos.df= of(agents=fish, var=c("who", "xcor", "ycor")) %>%  as.data.frame()
    
    radius4 <- left_join(radius_to_keep, whos.df) %>%  
      #mutate(foodGo = if_else(pycor > ycor, foodGo*1.5, foodGo)) %>%# increase foodcue
      ##avoid going to grandbanks at the start of the season with min tempGo.
      filter(!is.na(nafoGo), tempGo > 0.1 ,tempGo > tempHere, foodGo > foodHere & prob > prob_moving & breed %in% c("adult", "juvenile")) %>%
      #mutate(#foodGo = foodGo/max(foodGo),
      #     Go = (foodGo *100) + (tempGo *100)) %>% 
      dplyr::select(pxcor, pycor, who, tempGo, foodGo, zooGo) 
    radius5 <- neighOK %>%
      filter(!who %in% c(radius4$who)) %>%
      dplyr::select(pxcor, pycor, who)
    
    radius4b <- radius4 %>%
      group_by(who) %>% arrange(foodGo) %>% 
      # slice_max(order_by = foodGo, n = 1, with_ties=F) %>%
      slice_head( n = 100) %>%
      # slice_max(order_by = zooGo, n = 1, with_ties=F) %>%
      arrange(zooGo) %>% #takes the first 8, then choose randomly
      slice_head( n = 3) %>% #takes the first 10, then choose randomly
      slice_sample(n = 1) %>%
      dplyr::select(-tempGo, -foodGo)
    radius5b <- radius5 %>%
      group_by(who) %>%
      slice_sample(n = 1) %>%
      distinct()
    
    who_to_where <- as.matrix(
      bind_rows(
        radius4b,
        radius5b
      )
    )
    
    # MOVE
    # esxport fish at each time step.
    # also export
    
    if (nrow(who_to_where > 0)) {
      turtles_to_move <- turtle(fish, who = who_to_where[, "who"])
      turtlesMoved <- moveTo(turtles_to_move, who_to_where[, 1:2])
      fish <- turtleSet(turtlesMoved, other(agents = fish, except = turtlesMoved))
    }
    
    fish<- sortOn(agents = fish, var = "who")
    fish.df1 <- larvae_feeding(fish = turtle(fish, who=fish$who, breed="larvae"), seaworld = seaworld, d.d. = d.d., param = param, temp.var = temp.var, zoo.var = zoo.var, time_step=time_step)
    fish.df2 <- adult_feeding(fish = turtle(fish, who=fish$who, breed="adult"), seaworld = seaworld, d.d. = d.d., param = param, temp.var = temp.var, zoo.var = zoo.var, time_step=time_step)
    if(nrow(fish.df2) >0) fish.df2 <- spawning(fish.df2, time= dates[t], param=param, time_step=time_step)
    fish.df3 <- adult_feeding(fish = turtle(fish, who=fish$who, breed="juvenile"), seaworld = seaworld, d.d. = d.d., param = param, temp.var = temp.var, zoo.var = zoo.var, time_step=time_step)
    fish.df4 <- if("eggs" %in% fish$breed){turtle(fish, who=fish$who, breed="eggs")@.Data %>%  as_tibble()
    } else{  data.frame()}
    
    fish.df<- bind_rows(fish.df1, fish.df2, fish.df3, fish.df4) %>%  mutate(t=t)
    fish.df[is.na( fish.df)] <- 0
    fish.df <-fish.df %>%  mutate(energy.reserve=pmax(energy.reserve, 0))
    #add eggs
    
    if(!nrow(fish.df) == nrow(fish)) stop("Missing fish")
    
    fish.df <- left_join(fish.df, key.nafo)
    nafo.areas<-  expand_grid(age= unique(fish@.Data[,"age"]), UnitArea = unique(key.nafo$UnitArea))
    
    fish.df2<- left_join(fish.df %>%  mutate(fage=floor(age)), full_join(nafo.areas, fish.df) %>% 
                           mutate(N=replace_na(N, 0), fage=floor(age)) %>%  group_by(fage, UnitArea) %>%
                           summarize(N= sum(N, na.rm=T)) %>% ungroup() %>%  group_by(fage) %>%  mutate(NAFO=N/sum(N, na.rm=T)) %>%  dplyr::select(-N) %>%  pivot_wider(names_from=UnitArea, values_from=NAFO)) %>% 
      mutate_at(vars(fage: last_col()),function(x) replace_na(x, 0))
    
    fish.df <- fish.df %>% arrange(who)
    
    fishDyn_list[[t]] <- fish.df2 %>%
      group_by(fage) %>%
      summarize(
        COG_x = sum(xcor * N) / sum(N),
        COG_y = sum(ycor * N) / sum(N),
        length = weighted.mean(length, w = N),
        weight = weighted.mean(weight, w = N),
        N = sum(N),
        across(`1F`:last_col(), mean)
      ) %>%
      mutate(
        year = my.year,
        timestep = t,  # ← Ajouter colonne temps
        date = dates[t],
        replicate = replicate_id  # ← Ajouter réplicat
      )
    
    # variables necessary in the next step.
    fish <- NLset(turtles = fish, agents = fish, var = "NAFO", val = fish.df$NAFO)
    fish <- NLset(turtles = fish, agents = fish, var = "weight", val = fish.df$weight)
    fish <- NLset(turtles = fish, agents = fish, var = "batch", val = fish.df$batch)
    fish <- NLset(turtles = fish, agents = fish, var = "length", val = fish.df$L)
    fish <- NLset(turtles = fish, agents = fish, var = "Wstd", val = fish.df$Wstd)
    fish <- NLset(turtles = fish, agents = fish, var = "Wstruct", val = fish.df$Wstruct)
    fish <- NLset(turtles = fish, agents = fish, var = "energy.reserve", val = fish.df$energy.reserve)
    
    ##releasing eggs####
    eggs<- fish.df %>%  filter(state==3) %>%  group_by(who, xcor,ycor) %>%  
      summarize(N=sum(realised.fecundity*N)*0.5) %>% ungroup() #0é5 for female? added N on october 2025
    
    fish<- hatch(fish, who=eggs$who, n=1, breed="eggs")
    
    whos<- of(agents=fish, var="who") 
    new.who=NLwith(fish, var="who", val=whos[!whos %in% fish.df$who])
    
    fish <- NLset(turtles = fish, agents = new.who, var = "N", val = eggs$N)
    fish <- NLset(turtles = fish, agents = new.who, var = "length", val = rep(0.1, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "weight", val = rep(0.001, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "Wstd", val = rep(0.001, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "Wstruct", val = rep(0.001, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "maturity", val = rep(0, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "age", val = rep(0, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "F", val = rep(0, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "state", val = 1)
    fish <- NLset(turtles = fish, agents = new.who, var = "energy.reserve", val = rep(0, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "batch", val = rep(0, nrow(new.who)))
    fish <- NLset(turtles = fish, agents = new.who, var = "R", val = rep(0, nrow(new.who)))
    #mortality of eggs and speed of development with temperature
    
    #View(fish@.Data)
    popSim_list[[t]] <- inspect(turtles = fish, who = fish$who) %>%
      as_tibble() %>%
      mutate(
        year = my.year,
        timestep = t,
        date = dates[t],
        replicate_id = replicate_id
      )
  }
  fishDyn_df <- bind_rows(fishDyn_list, .id = "timestep")
  popSim_df <- bind_rows(popSim_list, .id = "timestep")
  
  # ===== RETOUR STRUCTURÉ =====
  return(list(
    replicate_id = replicate_id,
    my.year = my.year,
    fishDyn = fishDyn_df,
    popSim = popSim_df
  ))
}
