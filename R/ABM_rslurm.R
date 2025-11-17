source("R/utils/loadpackages.R")
source("R/MACKNWAN_ABM.R")
source("R/extract_time_series.R")

# Configuration
nReplicate <- 3
years <- 1999:2022
simu.name <- "simu6"

if(dir.exists(paste0("results/", simu.name))){
  stop("folder already exists")
} else {
  dir.create(paste0("results/", simu.name))
  dir.create(paste0("results/", simu.name, "/stats/"))
}

# Define wrapper function for rslurm
run_single_sim <- function(replicate_id, my.year, model_res, time_step, 
                           barrier, d.d., prob_moving, param_to_modify, 
                           percent_change) {
  # Call your simulation function
  result <- do.call(run_sim_ibm, 
                    list(replicate_id = replicate_id,
                         my.year = my.year,
                         model_res = model_res,
                         time_step = time_step,
                         barrier = barrier,
                         d.d. = d.d.,
                         prob_moving = prob_moving,
                         param_to_modify = param_to_modify,
                         percent_change = percent_change))
  return(result)
}

# Loop through years
for(y in years) {
  
  # Create parameter grid
  runparams <- expand.grid(
    replicate_id = seq_len(nReplicate),
    my.year = y,
    model_res = 30,
    time_step = 7,
    barrier = TRUE,
    d.d. = TRUE,
    prob_moving = 0.05,
    param_to_modify = "h",
    percent_change = 100
  ) %>%  asplit(1)
  
  cat("Lancement de", nReplicate, "réplicats via SLURM...\n")
  start_time <- Sys.time()
  
  # Submit jobs to SLURM
  sjob <- slurm_map(
    f = run_single_sim,
    x = runparams,
    jobname = paste0(simu.name, "_", y),
    nodes = 1,
    cpus_per_node = nReplicate,
    submit = TRUE,
    slurm_options = list(
      time = "02:00:00"
       )
    
  )
  
  # Wait for jobs to complete
  results <- get_slurm_out(sjob, outtype = "raw")
  
  elapsed <- Sys.time() - start_time
  cat("\n✓ Simulation terminée en", format(elapsed, digits = 2), "\n")
  
  # ===== POST-TRAITEMENT =====
  
  # Extract results from all replicates
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
  
  # Save results
  year.name <- ifelse(length(y) > 1, paste0(min(y), "_", max(y)), y)
  
  save(fishDyn_all, popSim_all, 
       file = paste0("results/", simu.name, "/Simrep_", year.name, ".RData"))
  
  if(!file.exists(paste0("results/", simu.name, "/runparams.RData"))) {
    save(runparams, file = paste0("results/", simu.name, "/runparams.RData"))
  }
  
  # Clean up SLURM job files
  cleanup_files(sjob)
}

# Extract results
extract_results_y(sim.name = simu.name)
