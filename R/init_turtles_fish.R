# Create the initial population using the NetLogoR package
# Create an agentMatrix object
# Individual IDs are "who" in the agentMatrix and starts at 0 (automatically created when creating the individuals)

#source("R/utils/length_weight_relationship.R")

#if !is.null turtles use turtles not random. 
createFish.y <- function(my.year,land=F, turtles=F, where="ovw", multiply.fish=1, plot=T, param=paramInit(),
                         model_res=10){
  seascape_mask=readRDS(paste0("data/polygons/",model_res,"km_seascape_mask.RDS"))
  r<- readRDS("data/polygons/reference_raster.RDS")
  r_newres= r
  res(r_newres) <- model_res *1000
 
  
   waa <- read_csv("../../iml-mackerel/04.0_weight-at-age/csv/waa_2024_base_cv1shrink0.csv")
  #laa needs to be updated for 2023-2024......
  laa <- read_csv("../../iml-mackerel/12.0_length-at-age/csv/laa_2022_base_cv1shrink0.csv")
  maa <-  read_csv("../../iml-mackerel/06.0_maturity-at-age/csv/2024/maa2024_base_smooth0.5.csv")
  
  f= read_csv("../../iml-mackerel/00.0_model/csv/2024/f.csv") %>%  rename(year=`...1`)
  nrel <- read_csv("../../iml-mackerel/00.0_model/csv/2024/nrel.csv") %>%  rename(year=`...1`) # proportion of N
  nabs <- read_csv("../../iml-mackerel/00.0_model/csv/2024/n.csv") %>%  rename(year=`...1`) # total N for density dependence
  laay = laa %>% pivot_longer(2:11, names_to="age", values_to="length") %>% dplyr::filter(year==my.year)
  maay = maa %>% pivot_longer(2:11, names_to="age", values_to="maturity") %>% dplyr::filter(year==my.year)
  waay = waa %>% pivot_longer(2:11, names_to="age", values_to="weight") %>% dplyr::filter(year==my.year)  %>%  mutate(weight=weight*1000) # in grams
  nrely = nrel %>% pivot_longer(2:11, names_to="age", values_to="nrel")%>% dplyr::filter(year==my.year)
  nabsy = nabs %>% pivot_longer(2:11, names_to="age", values_to="nabs") %>% dplyr::filter(year==my.year) %>%  
    group_by(year) %>%  summarize(N=sum(nabs*1000))
  fy = f %>% pivot_longer(2:11, names_to="age", values_to="F") %>% dplyr::filter(year == my.year) %>% 
    mutate(`F`= `F`/365)

  initPopDF<- list(laay, waay,maay,nrely, nabsy, fy) %>%  purrr::reduce(left_join) %>%  
  tidyr::uncount(round(nrel)*multiply.fish)   #rowid_to_column("ID") %>% #automatically generated
  
  initPopDF<-initPopDF %>% 
  dplyr::select(-year, -nrel) %>%  as.data.frame() %>%  
  dplyr::mutate(N=N/100*multiply.fish,# because there are 100 turtles
         Mstd = (param$a.lw * (length ^ param$b.lw)),
          Mstruct=Mstd *0.76,
         maturity=rbinom(n=nrow(initPopDF), size=1,prob = maturity),
         state=if_else(maturity==0, 1,2),
         age=as.numeric(as.character(age))) 
 #state is coerced to numeric when transform to tibble later on.
  
  
  
  #from brick to stact to world
  seaworld<- raster2world(raster::stack(r_newres))
 
  # Initialize the model objects (i.e., land and wolves)
 
  if(land){
    fish <- createTurtles(n = nrow(initPopDF), world = seaworld) # create as many wolves as nrow(initPopWolf_DF) 
}
   if(turtles){
    ##use coords in createTurles for overwintering area!!########
  #overwintering area
     if(where =="ovw") poly<- st_read("data/polygons/overwintering/overwintering_area2.shp")
     #start at spawning
     if(where=="spawn") poly <-  st_read("data/polygons/overwintering/spawninggsl_area.shp")
     
  initcoord<- st_sample(st_intersection(poly %>%  st_transform(st_crs(seascape_mask)),seascape_mask), size=nrow(initPopDF))
  initcoord<- st_coordinates(initcoord) %>%  as.data.frame() %>%  st_as_sf(coords=c("X","Y"), crs=st_crs(seascape_mask)) %>%  st_transform(crs=st_crs(r_newres)) %>%  st_coordinates()
  #fish<- sf2turtles(initcoord) # using sf but coordinates were incorrect
  #change coordinates to world coordinates (cell number in column and row) substraction because the order in turtles and terra is flipped
  turtpos<-  data.frame(xcor=colFromX(r_newres, initcoord[,1]),  ycor=nrow(r_newres)-rowFromY(r_newres, initcoord[,2]))
  sample_heading = sample(c(1:90, 270:360),nrow(initPopDF), replace=T) # replace because sample is larger than choices

  fish <- createTurtles(n = nrow(initPopDF), coords=turtpos %>%  as.matrix(), heading=sample_heading, breed="adult") #heading=north
   }
  
  fish <- turtlesOwn(turtles = fish, tVar = "length", tVal = initPopDF[, "length"]) # define each individual characteristic
  fish <- turtlesOwn(turtles = fish, tVar = "weight", tVal = initPopDF[, "Mstd"])
  fish <- turtlesOwn(turtles = fish, tVar = "Wstd", tVal = initPopDF[, "Mstd"])
  fish <- turtlesOwn(turtles = fish, tVar = "Wstruct", tVal = initPopDF[, "Mstruct"])
  fish <- turtlesOwn(turtles = fish, tVar = "maturity", tVal = initPopDF[, "maturity"])
  fish <- turtlesOwn(turtles = fish, tVar = "age", tVal = initPopDF[, "age"])
  fish <- turtlesOwn(turtles = fish, tVar = "N", tVal = initPopDF[, "N"])
  fish <- turtlesOwn(turtles = fish, tVar = "F", tVal = initPopDF[, "F"])
  fish <- turtlesOwn(turtles = fish, tVar = "state", tVal = initPopDF[, "state"])
  fish <- turtlesOwn(turtles = fish, tVar = "energy.reserve", tVal = rep(0, nrow(initPopDF)))
  fish <- turtlesOwn(turtles = fish, tVar = "batch", tVal = rep(0, nrow(initPopDF)))
  fish <- turtlesOwn(turtles = fish, tVar = "R", tVal = rep(0, nrow(initPopDF)))
  fish <- turtlesOwn(turtles = fish, tVar = "NAFO", tVal = rep(1, nrow(initPopDF)))

  
  #coefficient from Boyd do not work
  #initPopDF %>%  mutate(Mst= 0.00285 *length ^ 3.325) %>%  ggplot(aes(x=weight, y=Mst))+geom_point() + geom_abline(slope=1, intercept = 0)
 #lwall= left_join(laa %>% pivot_longer(2:11, names_to="age", values_to="length") ,
#  waa %>% pivot_longer(2:11, names_to="age", values_to="weight")  %>%  mutate(weight=weight*1000)) # in grams
 #ggplot(data=lwall, aes(x=length, y=weight))+geom_point()  
#Length weight -relationship for standard weight
#nls_model <- nls(weight ~ a * length^b, data = lwall, start = list(a = 0.00285, b = 3.325))
#summary(nls_model)

#a=coefficients(nls_model)[[1]]
#b=coefficients(nls_model)[[2]]
#works well
   #initPopDF %>%  mutate(Mstd= a *length ^ b,
     #                    Mstruct=Mstd *0.76) %>%  ggplot(aes(x=weight, y=Mstd))+geom_point() + geom_abline(slope=1, intercept = 0)
  
  
  #fish <- turtlesOwn(turtles = fish, tVar = "degreedays", tVal = rep(0,nrow(initPopDF),))
  #fish coordinates = cell number in raster.
  write_rds(fish, paste0("data/seascape/fish_",model_res,"km_", my.year, ".RDS"))
  

}

#where has to be ovw or spawn

#dx and dy to accound for how much would change.
#Report the sine of the turtles' heading multiplied by the dist values. Heading 0 is north and angles are calculated in degrees in a clockwise manner



