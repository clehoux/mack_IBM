
nReplicate <- 2
years <- 2015

params <- expand.grid(
  replicate_id = seq_len(nReplicate),
  my.year = years,
  model_res = 30,
  time_step = 7,
  barrier = TRUE,
  d.d.=T
)

library(rslurm)

sjob <- slurm_apply(
  run_sim_ibm, 
  params, 
  jobname = 'fish_ibm_replicates', 
  nodes = nReplicate,
  cpus_per_node = 1 # one core per job
)

results <- slurm_collect(sjob)
output_df <- do.call(rbind, results) #check as list is multiple df.
print(output_df)


out_popSim  <- paste0("results/popSim_", model_res, "km_", step.name, my.year, "_rep", replicate_id, ".RDS")
out_fishDyn <- paste0("results/fishDyn_", model_res, "km_", step.name, my.year, "_rep", replicate_id, ".RDS")
write_rds(popSim,  out_popSim)
write_rds(fishDyn, out_fishDyn)
