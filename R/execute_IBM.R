#load package
source("R/utils/loadpackages.R")
#load utilities code
source("R/utils/basemap.R")
source("R/utils/movement.R") # probability of moving based on temperature (depth)

time_step=7
source("R/utils/parameters.R")

source("R/utils/1_1_download_data.R")
#download.glorys_daily(my.year=my.year)
download_sat(my.years=1999:2023, start_date="-01-01")
#check sat data
sat.files<- list.files("data/sat/daily")
table(substring(sat.files, 12,15))

source("R/seascape.R")
source("R/init_turtles_fish.R")
for(my.year in 1999:2022){ 
  for(mres in c(30)){
seascape_y(start_date=paste0(my.year, "-01-01"), end_date=paste0(my.year, "-12-31"), time_step=time_step, model_res=mres)
    createFish.y(my.year=my.year, land=F, turtles=T, where="ovw", multiply.fish = 1,param=paramInit(time_step = time_step), model_res=mres)
  }
}
###MODELS#####
#change simu.name and parameters and source
source("R/ABM_furrr.R")



#####RESULTS#####
source("R/Rmd/render_time_series.R")
render_time_series(simu.name="simu5")
render_time_series(simu.name="simu6")
render_time_series(simu.name="simu7")
