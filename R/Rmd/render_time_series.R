render_time_series = function(simu.name) {
  rmarkdown::render(
    "R/Rmd/IBM_results_time_series.Rmd", params = list(
      simu.name=simu.name
    ),
    output_dir="R/Rmd/",
    output_file = paste0("IBM_results_time_series_", simu.name, ".html")
  )
}