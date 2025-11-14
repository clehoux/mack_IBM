#load package
source("R/utils/loadpackages.R")

extract_results_y <-  function(sim.name){

fs<- list.files(paste0("results/", sim.name, "/"), pattern="Simrep", full.names = T)
load(fs[1]) # for runparams
fishDyn_combined <- map_df(fs, ~{
  load(.x)
  fishDyn_all %>%
    mutate(week = lubridate::week(date)) %>%  filter(week <= 52)
})

popSim_combined <- map_df(fs, ~{
  load(.x)
  popSim_all%>%
    mutate(week = lubridate::week(date)) %>%  filter(week <= 52)
})


load(paste0("results/",sim.name,"/runparams.RData"))
model_res<- runparams$model_res[1]

########TEP and TEP by regions########
egg.df<- popSim_combined %>%  filter(breed=="eggs")
key.nafo<- readRDS(paste0("data/polygons/",model_res,"km_", "keynafo.RDS"))

egg_region <-  left_join(egg.df, key.nafo) %>% 
  mutate(region= if_else(grepl(UnitArea, pattern="2")| UnitArea=="1F"|grepl(UnitArea, pattern="3"), "eNL",
                         if_else(UnitArea=="4R", "wNL",
                                 if_else(UnitArea=="4S", "nGSL",
                                         if_else(UnitArea=="4T", "sGSL", "SS"))))) %>% 
  group_by(region, week, year, replicate_id) %>%  summarize(WEP=sum(N))  


egg_total_region <- egg_region %>%
  group_by(region, year, replicate_id) %>%  summarize(TEP=sum(WEP)) #%>% 
egg_total_region %>% ggplot(aes(fill=region, x=as.factor(year),  y=TEP))+geom_bar(stat="identity") +facet_wrap(~replicate_id)

egg_weekly <-  egg_region %>%  ungroup() %>%  group_by(week, year, replicate_id) %>%  summarize(WEP=sum(WEP))#%>%  
egg_weekly %>%  ggplot(aes(x= as.factor(week), col=as.factor(year),  y=WEP))+geom_boxplot()
egg_weekly %>%  ggplot(aes(x= week, col=as.factor(year),  y=WEP))+geom_point() +geom_smooth() +facet_wrap(~year, scales="free")

 egg_total <-  egg_region %>%  ungroup() %>%  group_by(year, replicate_id) %>%  summarize(TEP=sum(WEP)) #%>%  
 egg_total  %>%  mutate(TEP=TEP/10^9)%>% ggplot(aes(x= as.factor(year), y=TEP))+geom_boxplot()
 #same scale as model, fecundity equation OK 
 egg_season<- left_join(egg_weekly, egg_total) %>%  mutate(prop.egg=WEP/TEP)
 
 
egg_peak <-   egg.df %>%  ungroup() %>%  group_by(date, year, replicate_id) %>%  summarize(DEP=sum(N)) %>% ungroup() %>% 
    group_by(year, replicate_id)  %>% dplyr::slice_max(DEP) %>%  dplyr::select(-DEP)

  save(egg.df,egg_region, egg_weekly, egg_total, egg_total_region,egg_peak, file=paste0("results/", sim.name, "/stats/Eggs_time_series.Rdata"))
  
  #virtual egg survey   
  #setup a virtual survey date = real survey or survey date = constant on June 21.
  #to make it pit, should be included in the seascape???
#proportion of fish that are spawning  using popSim_combined
  #load postition of grids
  
 
 r<- readRDS("data/polygons/reference_raster.RDS")
 r<- as(r, "SpatRaster")
  r_newres2= r
  res(r_newres2) <- model_res *1000
  
  
  look<-  read.delim("C:/LEHOUX/Maquereau/iml-mackerel/03.0_egg-index/data/lookup_station_egg.txt")
  egg_sf<- look %>%  dplyr::select(station,latitude, longitude) %>%  
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%  st_transform(st_crs(r_newres2))

 egg_raster <- terra::rasterize(vect(egg_sf) , r_newres2, 
                                field="station", fun="first")
 egg_raster_fil <- terra::focal(egg_raster, w=3, fun="modal", na.rm=TRUE, 
                                   na.policy="only")



egg_raster_fil
nrow(unique(egg_raster_fil))# 9.4 and 9.5 are in the same cell.
plot(egg_raster_fil)
eggworld <- raster2world(raster(egg_raster_fil))
 
egg.df$station <-  of(world=eggworld,agents = egg.df[,c("xcor", "ycor")] %>%  as.matrix())

#total as in total
virt1<- egg.df %>%  filter(!is.na(station)) %>%  group_by(year, replicate_id) %>%  summarize(TEP=sum(N)) 
ggplot(virt1, aes(x=as.factor(year), y=TEP)) +geom_boxplot()

#our survey dates 
survd<- read.delim("C:/LEHOUX/Maquereau/iml-mackerel/03.0_egg-index/results/2024/Survey_dates.txt")
survw= survd %>%  mutate(survey.week= week(as.Date(paste0(year,"-", gsub(date.med, pattern="'", replacement=""))))) %>%  dplyr::select(year, survey.week)
check.eggs<- left_join(egg.df %>%  filter(!is.na(station)), survw) %>%  filter(week==survey.week) %>% 
  group_by(year, replicate_id, week) %>%  summarize(TEP_survey=sum(N)) 

virt1b<- left_join(left_join(virt1, check.eggs) %>% mutate(prop_survey=TEP_survey/TEP), egg_season %>%  dplyr::select(-WEP, -TEP)) %>% 
  mutate(virtual.TEP= TEP_survey / prop.egg)
library(ggpmisc)
virt1b %>% ggplot(aes(x=TEP/10^9, y=virtual.TEP/10^9, col=year)) +geom_point() +
  facet_wrap(~replicate_id)+
  scale_y_continuous(trans="log10p1") +  scale_x_continuous(trans="log10p1") +
  geom_smooth(method="lm")+stat_poly_eq(aes(label = paste(after_stat(eq.label), 
                                                          after_stat(rr.label), 
                                                          sep = "~")),
                                        formula = y ~ x, 
                                        parse = TRUE,
                                        size = 3)

#TEP eval
TEP.eval<- read.csv("C:/LEHOUX/Maquereau/iml-mackerel/03.0_egg-index/csv/2024/TEP_2024.csv")

allTEP<- left_join(TEP.eval %>%  rename(TEP.eval=TEP), 
          virt1b %>% group_by(year) %>% 
            summarize(TEP=mean(TEP),
                      virtual.TEP=mean(virtual.TEP)))
library(ggpmisc)
allTEP %>% ggplot(aes(x=TEP.eval/10^9, y=virtual.TEP/10^9, col=year)) +geom_point() +
  scale_y_continuous(trans="log10p1", breaks=c(10,100,200,300)) +
  scale_x_continuous(trans="log10p1", breaks=c(10^4,10^5)) +
  geom_smooth(method="lm")+stat_poly_eq(aes(label = paste(after_stat(eq.label), 
                                                          after_stat(rr.label), 
                                                          sep = "~")),
                                        formula = y ~ x, 
                                        parse = TRUE,
                                        size = 3)



save(virt1b, allTEP, file=paste0("results/", sim.name, "/stats/Eggs_Virtual_ecologist.Rdata"))



#### Recruitment########
recr<- fishDyn_combined %>%  filter(fage==0, week==52) %>%  dplyr::select(N, year, replicate) %>% 
  rename(Recruitment= N)#%>%  
 recr %>% ggplot(aes(x= as.factor(year), y=Recruitment))+geom_boxplot()

#SSBiomass # good but mostly driven by N in the model.
fishDyn_combined %>% filter(fage >=2, week %in% c(1,21, 52)) %>%  mutate(biomass=N*weight) %>% 
  group_by(replicate, year, week) %>%  summarize(SSB=sum(biomass))%>%  
ggplot(aes(x= as.factor(year), y=SSB, col=as.factor(week)))+geom_boxplot()

ssb<- fishDyn_combined %>% filter(fage >=2, week %in% c(21)) %>%  mutate(biomass=N*weight) %>% 
  group_by(replicate, year, week) %>%  summarize(SSB=sum(biomass))#%>%  
  ssb %>% ggplot(aes(x= as.factor(year), y=SSB))+geom_boxplot()

 
####Kn#### 
Kn<-  popSim_combined %>%  mutate(Kn=weight/Wstd) %>% group_by(year, week) %>%  summarize(Kn=mean(Kn))
Kn %>%  ggplot(aes(x=week, y=year, fill=Kn)) +geom_tile() + scale_fill_viridis_c(option="turbo")
###Kn#### 
Knall<-  popSim_combined %>%  mutate(Kn=weight/Wstd) %>% group_by(year) %>%  summarize(Kn=mean(Kn))
Knall %>%  ggplot(aes(x=year, y=Kn)) +geom_line() 
#usage of regions

key.nafo<- readRDS(paste0("data/polygons/",model_res,"km_", "keynafo.RDS"))

  fish_region<- left_join(popSim_combined %>%  filter(age!=0), key.nafo) %>% 
  mutate(region= if_else(grepl(UnitArea, pattern="2")| UnitArea=="1F"|grepl(UnitArea, pattern="3"), "eNL",
                         if_else(UnitArea=="4R", "wNL",
                                 if_else(UnitArea=="4S", "nGSL",
                                         if_else(UnitArea=="4T", "sGSL", "SS"))))) %>% dplyr::ungroup() %>% 
     dplyr::group_by(region, week, year, replicate_id) %>%  dplyr::summarize(N=sum(N, na.rm=T)) %>%  #sum across age
  pivot_wider(names_from=region, values_from=N, values_fill=0) %>% dplyr::ungroup() %>% 
  mutate(total= SS+eNL+nGSL+sGSL+wNL+`NA`,
         SS=SS/total,
         eNL=eNL/total, 
         nGSL=nGSL/total, 
         sGSL=sGSL/total,
         wNL=wNL/total,
         `NA`=`NA`/total,
         totalprop=SS+eNL+nGSL+sGSL+wNL+`NA`) %>% #for validation
  dplyr::select(-totalprop, -total) %>%  pivot_longer(c(SS:wNL, `NA`))

fish_region %>%  filter(replicate_id==3) %>% 
  ggplot(aes(x=as.factor(week), y=value, fill=name))+
  geom_bar(stat="identity") +
  facet_wrap(~year)
#proportion during survey?
waa <-  fishDyn_combined %>% filter(week==26) %>%  dplyr::select(fage, year, replicate, weight) 
laa <-  fishDyn_combined %>% filter(week==26) %>%  dplyr::select(fage, year, replicate, length) 

save(fishDyn_combined, popSim_combined, recr, ssb,Kn, Knall,fish_region, waa, laa,  file=paste0("results/", sim.name, "/stats/PopDyn.Rdata"))



#####Vritual fishery and sample collection protocol for S52?#####
##pas sur que ça représente quelque chose. 
fish_region<- left_join(popSim_combined %>%  filter(age!=0), key.nafo) %>% 
  mutate(region= if_else(grepl(UnitArea, pattern="3K")|grepl(UnitArea, pattern="3L"), "eNL",
                         if_else(UnitArea=="4R", "wNL",
                                 if_else(UnitArea=="4S", "nGSL",
                                         if_else(UnitArea=="4T", "sGSL",
                                                 if_else(UnitArea %in% c("4V","4T", "4W", "4X", "5Y", "5Z"),"SS", 
                                                         if_else(UnitArea %in% c("3M", "3N", "3O","3P"), "sNL", NA)))))))
s52.date= c("2025-05-15",
            "2025-05-25",
            "2025-06-05",
            "2025-06-15",
            "2025-06-25",
            "2025-07-05",
            "2025-07-15",
            "2025-07-25",
            "2025-08-15",
            "2025-09-15",
            "2025-10-10")
s52.week = week(as.Date(s52.date))

ve<- read.delim("C:/LEHOUX/Maquereau/Recherche/mack_IBM/data/virtual_ecologist_S52.txt")

sampled_data <- fish_region %>% filter(age >=1) %>% 
  inner_join(ve, by = c('region', 'week')) %>%
  group_by(year, replicate_id, region, week, Nsample) %>%
  do({
    n_to_sample <- first(.$Nsample)
    slice_sample(., n = min(n_to_sample, nrow(.)), replace = FALSE)
  }) %>% ungroup() %>% 
  select(-Nsample) 
#verification 
sampled_data %>%
  group_by(year, replicate_id, region, week) %>%
  summarise(count = n(), .groups = 'drop')

sampled_data %>%  group_by(region, year, replicate_id) %>%  summarize(mage=mean(age, na.rm=T)) %>% 
  ggplot(aes(x=as.factor(year), y=mage, col=region)) + geom_boxplot()

sampled_data %>%  group_by(year, replicate_id) %>%  summarize(mage=mean(age, na.rm=T)) %>% 
  ggplot(aes(x=as.factor(year), y=mage)) + geom_boxplot()

save(sampled_data, file=paste0("results/", sim.name, "/stats/S52_Virtual_ecologist.Rdata"))


}


