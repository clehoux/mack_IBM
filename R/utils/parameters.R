
paramInit<- function(time_step, param_to_modify = NULL, percent_change = 0){

nls_lw<- readRDS("data/utils/nNWAM_Length_weight_nls.RDS")
  
  
param<- list(
a_v=0.62, # ; Sambilay Jr (1990, equation 1)
b_v=0.35, # ; Sambilay Jr (1990, equation 1)
Ar=4.01,
A =0.15  , #
Cmax=0.69 ,
Ea=0.5,
Tref=285.15, #Reference temperature for the energy budget (12 °C or 285.15 k)
h=0.882, # see silby et al increase to high effect of food density  #warning not same value in Boyd 2018 and 2020
boltz = (8.62 * (10 ^ -5)), #K 
dd=3.6e-13,# c in boyd, strenght of the predator density dependence
M.a=0.28/365 *time_step ,#daily mortality rate.
M.e= 0.281 ,# from boyd egg and larvae background mortality
Ae=0.95,#assimilation efficiency
ez = 27.9,#energy density of zooplancton
S0 = 0.45e8,
A0=8.86e7,
k=0.314 /365 ,# taux annuel
Loo = 42.4,#asymptotic lenght
Loj = 25,#asymptotic lenght for juveniles
Ef =  7,  # energy content of flesh kJ/g
El =14.7,    # Energy costs of synthesizing lipid 
E=39.3,#Energy content of lipid (EL sur le code en ligne)
Fs=3.60,    #energy costs of synthesising flesh kJ/g
a.lw=coefficients(nls_lw)[[1]],
b.lw=coefficients(nls_lw)[[2]],
# Spawning coefficient
a.f = 8.80, # normalizing constant for fecundity Lockwood 1981
b.f =  3.02, #scaling exponent for fecundity Lockwood
egg.mass= 0.001, #(Sibly et al. 2015)
nbatch= 8,
Bint =7,
k1=0.025 # by day max growth rate
)
if (!is.null(param_to_modify) && param_to_modify %in% names(param)) {
  param[[param_to_modify]] <- param[[param_to_modify]] * (1 + percent_change / 100)
}
 return(param)
 }



