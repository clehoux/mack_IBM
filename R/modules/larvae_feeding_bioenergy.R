larvae_feeding<- function(fish, seaworld, d.d., param=param, temp.var, zoo.var, time_step){

Nfish <- length(unique(fish@.Data[, "who"]))
if(Nfish >0){
whos= of(agents=fish, var="who") 

fish.df <- fish@.Data %>%  as_tibble() %>% 
  mutate(
    ###Calculate energy budget##
    SST= of(world = seaworld, agents = coordinates(fish), var=temp.var),
    X= of(world = seaworld, agents = coordinates(fish), var=zoo.var)/1000/0.2,#mg to grams to WW transformation (Brey et alé 2001)
    NAFO= of(world = seaworld, agents = coordinates(fish), var="nafo"),
    A_sst=exp((-1*param$Ea)/param$boltz *((1/SST)-(1/param$Tref )) ),
    arrhenius = exp((-1 * param$Ea)/(param$boltz*SST))
  )
if(d.d.){ 
  pHere <- patchHere(world = seaworld, turtles = fish)
  dd_who <- cbind(who = whos, pHere, inspect(turtles = fish, who = whos)[, c("N", "weight")])
  dd_Here <- dd_who %>%
    as.data.frame() %>%
    group_by(pxcor, pycor) %>%
    mutate(n = n(), biomass = n / Nfish * N * weight)
  D = dd_Here$biomass
  dd.factor=param$dd * D } else(dd.factor=rep(0, Nfish))


fish.df  <-  bind_cols(fish.df, dd.factor=dd.factor) %>%  
  mutate(
    IR.g=param$Cmax *A_sst * (X/(X+param$h+dd.factor)) * weight^(2/3) *time_step, # (IR = g/day) *27.9 kJ/g
    energy.assimilated=IR.g * param$Ae * param$ez,
    SMR=param$S0 * (weight ^0.75) *  arrhenius *time_step,
    
    #AMR <- to implement later with swimming speed
    energy.assimilated2 = if_else(energy.assimilated > SMR, energy.assimilated-SMR, 0),
    energy.reserve = if_else(energy.assimilated < SMR, energy.reserve-SMR, energy.reserve),
    
    max.growth.rate = ((param$k) * 2) * exp((-1* param$Ea / param$boltz) * ((1 / (SST)) - (1 / param$Tref))) * (param$Loo - length),
    max.growth.rate = if_else(max.growth.rate < 0 , 0,max.growth.rate*time_step),
    #cm -jour
    possibleL =  length + max.growth.rate,
     growth.costs =((param$a.lw * (possibleL ^ param$b.lw)) - Wstruct) * (param$Fs + param$Ef) *time_step,
    growth.rate = if_else(energy.assimilated2 >= growth.costs, max.growth.rate, 
                          (max.growth.rate / growth.costs) * energy.assimilated2),
    L= length+ growth.rate,
    Wstd = (param$a.lw * (L^ param$b.lw)),
   # Wstruct = Wstd * 0.76,
    energy.assimilated3= if_else(energy.assimilated2 >= growth.costs, 
                                 energy.assimilated2 - growth.costs,
                                 0),
    #storage                             
    energy.reserve= 0,
   energy.reserve.max=0,
   spawning_prob=0,
   maintenance.energy=SMR,
   potential.fecundity=0, max.R=0, max.batch.R=0, realised.fecundity=0, IGS=0,
    gonad.mass=0, 
    weight = Wstd, 
    FK = 100 * (weight / (length ^ 3)))                        # ; Fulton's condition factor
} else(fish.df =data.frame())
    
    return(fish.df)

  }