V_min=function(L, param =paramInit()){#Vmin is in km/hr
  #turtles length.
  param$A * (L ^ (param$a_v)) * (param$Ar ^ (param$b_v)) 
}      # ; Sambilay Jr (1990, equation 1)

length_to_distance<- function(turtle, grid.resolution=10, param =paramInit()){
vmin <- V_min(turtle$length, param=param)
realised_speed = vmin + ((vmin)  * runif(1))    # we add some random noise to Vmin to get realised speed to account for vertical movement and finer scale deviations
distance24h =  (realised_speed * 24) /grid.resolution#(speed per day) nb cellule/day
distance24h
}

#fonction de suitabilité pour la température basé sur les oeufs
prob_temp <-  function(gotemp){
  dnorm(gotemp, mean=11, sd=2)/0.2
}

prob_temp_spawn <-  function(gotemp){
  dnorm(gotemp, mean=11, sd=1)/0.4  # max at dnorm(11, mean=11, sd=2)
}

#data.frame(temp =rnorm(1000, mean=11, sd=1)) %>% 
#  ggplot(aes(x=temp))+geom_density() +theme_few()
#how the function was found
#temperature suitabilty#

#eggs<- readRDS("C:/LEHOUX/Maquereau/iml-mackerel/03.0_egg-index/data/2024/PL_Bongo_Scomber_eggs_larvae_Counts_L2_2024.RDS")

#eggs %>% filter(trajet==1) %>%  summarise(mean=weighted.mean(temperature0_10, w=maq_eggs_stage1, na.rm=T),
#                                         sd=sd(temperature0_10, na.rm=T))
#max.prob = dnorm(11, mean=11, sd=2)
#dnorm(7, mean=11, sd=2)/max.prob  probability of moving. 0.5 at 11 then *2 for maximum probability
#dnorm(10, mean=11, sd=2)/max.prob
#dnorm(11, mean=11, sd=2)/max.prob
#dnorm(12, mean=11, sd=2)/max.prob
#dnorm(16, mean=11, sd=2)/max.prob

prob_depth <-  function(godepth){
  # plot(density(rgamma(n=1000, shape=2, scale=30)))
  dgamma(godepth, shape=2, scale=30)
}

#data.frame(depth =rgamma(1000, shape=2, scale=30)) %>% 
#  ggplot(aes(x=depth))+geom_density() +theme_few()
