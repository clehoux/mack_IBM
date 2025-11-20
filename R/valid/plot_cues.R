library(tidyverse)#fonction de suitabilité pour la température basé sur les oeufs
library(ggthemes)
prob_temp <-  function(gotemp){
  dnorm(gotemp, mean=11, sd=2)/0.2
}

prob_temp_spawn <-  function(gotemp){
  dnorm(gotemp, mean=11, sd=1)/0.4  # max at dnorm(11, mean=11, sd=2)
}

data.frame(temp =seq(5,20, 0.5)) %>% mutate(Movement =prob_temp(temp),
                                            Spawning= prob_temp_spawn(temp)) %>% 
  pivot_longer(Movement:Spawning) %>% 
  ggplot(aes(x=temp, y=value, col=name))+geom_point() +geom_line(linewidth=3, alpha=0.5) +theme_few() + 
  ylab("Preference") +xlab("Temperature \u00B0C") +
  scale_color_manual(values=c("black", "grey"), name="") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.8))

#OR load parameters
Ea=0.5
Tref=285.15 #Reference temperature for the energy budget (12 °C or 285.15 k)
h=0.882 # see silby et al increase to high effect of food density  #warning not same value in Boyd 2018 and 2020
boltz = (8.62 * (10 ^ -5)) #K 
foodCue<- expand.grid(x=seq(5,20, 1),
                      y=c(8,14),
                      z=seq(0,5, 0.1)) %>%  mutate(temp=x, x=x+273, out=
                                                     (y / 12) * exp((-Ea / boltz) * ((1 / (x)) - (1 / Tref))) *
                                                     (z/1000/0.2) / (h + (z/1000/0.2) ))

foodCue %>%  ggplot(aes(x=temp, y=z, fill=out)) +facet_wrap(~y) +
  geom_tile() + scale_fill_viridis_c(option="turbo", guide=F) +theme_few() +
  scale_y_continuous(name="Calanus ssp. biomass g\u00B7m\u207B\u00B2")+
  xlab("Temperature \u00B0C")

# pour mettre y en perspective il faudrait voir c'est quoi le gradient en fonction de la latitude.....
