
source("R/utils/loadpackages.R")
source("R/MACKNWAN_ABM.R")
source("R/extract_time_series.R")


# Configuration
nReplicate <- 3
years <-1999:2022
simu.name = "simu6"
if(dir.exists(paste0("results/", simu.name))){stop("folder already exists")
  }else{
dir.create(paste0("results/", simu.name))
    dir.create(paste0("results/", simu.name, "/stats/"))

n_workers <- nReplicate * length(years)# 1 worker par réplicat/year # 1.2 Go par workers. donc on peut peut-être aller juqu'à 10? sur mon portable
# 1year =1.6 hours. 
# Paramètres
runparams <- expand.grid(
  replicate_id = seq_len(nReplicate),
  my.year = years,
  model_res = 30,
  time_step = 7,
  barrier = T,
  d.d. = TRUE,
  prob_moving = 0.05,#,#, #,#0.1=PROBABILITY OF MOVING OF 90%
  param_to_modify = "h",
  percent_change= 100
)

params_list <- split(runparams, seq(nrow(runparams)))

# Exécution
plan(multisession, workers = n_workers)
handlers(global = TRUE)

cat("Lancement de", nReplicate, "réplicats en parallèle...\n")
start_time <- Sys.time()

results <- future_map(
  params_list,
  ~do.call(run_sim_ibm, as.list(.x)),
  .progress = TRUE,
  .options = furrr_options(seed = 123)
)

elapsed <- Sys.time() - start_time
cat("\n✓ Simulation terminée en", 
    format(elapsed, digits = 2), "\n")

# ===== POST-TRAITEMENT =====

# Extraire les fishDyn de tous les réplicats
fishDyn_all <- map_df(
  results,
  ~bind_rows(.x$fishDyn),
  .id = "sim_id"
)

popSim_all <- map_df(
  results,
  ~bind_rows(.x$popSim),
  .id = "sim_id"
)
# Sauvegarder les résultats
year.name = ifelse(length(years) > 1 , paste0(min(years),"_", max(years)), years)


save(fishDyn_all, popSim_all, file=paste0("results/",simu.name,"/Simrep_",year.name,".RData"))
if(!file.exists(paste0("results/",simu.name,"/runparams.RData"))) save(runparams, file=paste0("results/",simu.name,"/runparams.RData"))

plan(sequential)
#1.5 hours with  barrier

extract_results_y(sim.name=simu.name)
}


#30 km vs 10 km does not reduce the time significantly 1.3 hours instead of 1.6 approx.
