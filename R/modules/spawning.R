
#prepare spawning
spawning <- function(fish.df, time, param, time_step){
 fish.df %>% 
  mutate(
    pre_prob=rbinom(prob=prob_temp_spawn(gotemp = SST - 273.15), size=1, n=nrow(fish.df)),
  spawning_prob = if_else((state == 2 | state == 3) & batch < 5 & yday(time) >150 & yday(time) < 225 & pre_prob==1 ,1 , 0),
  state= if_else(spawning_prob==1, 3, state),
  maintenance.energy = energy.reserve * 0.1,
  R = R + energy.reserve - (maintenance.energy * time_step),
  potential.fecundity = param$a.f * (L^param$b.f),
  max.R = potential.fecundity * param$egg.mass * (param$Ef + param$Fs),
  max.batch.R = max.R / param$nbatch / (param$Bint / time_step),
  realised.fecundity =(R / (max.batch.R * (param$Bint / time_step))) * potential.fecundity, #number of eggs
  #spawning and updating
  batch = if_else(realised.fecundity > 0 & state==3,batch + 1, batch), # no eggs released at the start of the spawning season
  energy.reserve = if_else(state==2 | state==3,energy.reserve - max.batch.R, energy.reserve),
  gonad.mass = if_else(state==2 | state==3, gonad.mass + (max.batch.R / (param$Ef + param$Fs)), gonad.mass),
  gonad.mass = if_else(realised.fecundity > 0 & state==3, 0, gonad.mass),
  R= 0,
  IGS = gonad.mass / weight,
  state= if_else(batch ==5, 1, state))
}

#to check egg overlap with zooplankton as a proxy of recutmenty, calanus spp or cfin.