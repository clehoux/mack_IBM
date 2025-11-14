adult_feeding<- function(fish, seaworld, d.d., param, temp.var, zoo.var, time_step){

Nfish <- length(unique(fish@.Data[, "who"]))
if(Nfish >0){
whos= of(agents=fish, var="who") 
breed.fish= unique(of(agents=fish, var="breed")) 


fish.df <- fish@.Data %>%  as_tibble() %>% 
  mutate(
    ###Calculate energy budget##
    SST= of(world = seaworld, agents = coordinates(fish), var=temp.var),
    X= of(world = seaworld, agents = coordinates(fish), var=zoo.var)/1000/0.2,#mg to grams to WW transformation (Brey et alé.2001)
    NAFO= of(world = seaworld, agents = coordinates(fish), var="nafo"),
    A_sst=exp((-1*param$Ea)/param$boltz *((1/SST)-(1/param$Tref )) ),
    arrhenius = exp((-1 * param$Ea)/(param$boltz*SST))#A certain form of the Arrhenius function is needed for metabolic rate calculations and is calculated her
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
    vmin= V_min(length, param=param),
    realised_speed = vmin + (vmin* runif(1)),    # we add some random noise to Vmin to get realised speed to account for vertical movement and finer scale deviations
    
    IR.g=param$Cmax *A_sst * (X/(X+param$h+dd.factor)) * weight^(2/3) *time_step, # (IR = g/day) *27.9 kJ/g
    energy.assimilated=IR.g * param$Ae * param$ez,
    SMR=param$S0 * (weight ^0.75) *  arrhenius *time_step,
    AMR=param$A0 * (weight ^ (0.75)) * realised_speed * arrhenius,
    MR= AMR,
    energy.assimilated2 = if_else(energy.assimilated > MR, energy.assimilated-MR, 0),
    energy.reserve = if_else(energy.assimilated < MR, energy.reserve-MR, energy.reserve),
    
    max.growth.rate = ifelse(breed.fish =="adult", param$k * exp((-1* param$Ea / param$boltz) * ((1 / (SST)) - (1 / param$Tref))) * (param$Loo - length),
                              param$k1 * exp((-1* param$Ea / param$boltz) * ((1 / (SST)) - (1 / param$Tref))) * length *log(param$Loj / length)),#else=juveniles  growth rate
    
    
    max.growth.rate = if_else(max.growth.rate < 0 , 0,max.growth.rate*time_step),
    #cm -jour
    possibleL =  pmin(length + max.growth.rate, if_else(age <1, param$Loj, param$Loo)),
    growth.costs =((param$a.lw * (possibleL ^ param$b.lw)) - Wstruct) * (param$Fs + param$Ef) *time_step,
    growth.rate = if_else(energy.assimilated2 * 0.5 >= growth.costs, max.growth.rate, 
                          max.growth.rate / growth.costs * (energy.assimilated2 * 0.5)),
    L= pmin(length+ growth.rate,if_else(age <1, param$Loj, param$Loo)),
    #standardL=(L - 0.1561) / 1.1396,
    Wstd = (param$a.lw * (L^ param$b.lw)),
    Wstruct = Wstd * 0.76,
    energy.assimilated3= if_else(energy.assimilated2 * 0.5 >= growth.costs, energy.assimilated2 - growth.costs,
                                 energy.assimilated2 * 0.5),
    #storage                             
    #juveniles and adults store any remaining energy as lipid
    energy.reserve.max= (Wstruct * 0.59) * param$E, #energy content of lopid # individual can store no more lipid than the equivalent of 78% of their structural mass (see TRACE for why)
    energy.reserve= pmin(if_else(energy.assimilated3 > 0, energy.reserve + (energy.assimilated3 * (param$E/(param$E+param$El))), 
                                  energy.reserve),energy.reserve.max),
    gonad.mass=0, 
    weight = Wstruct +(energy.reserve / param$E) + gonad.mass,#; total mass is the sum of structural, fat and gonad mass
    FK = 100 * (weight / (L ^ 3)))                        # ; Fulton's condition factor
} else(fish.df =data.frame())

return(fish.df)
  }