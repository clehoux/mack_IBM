run_sim_food_tibble <- function( my.years, model_res, time_step, 
                                temp.var="sst", zoo.var="zooplankton", 
                                param_to_modify = NULL, percent_change = 0) {
  
  source("R/utils/loadpackages.R")
  source("R/utils/movement.R")
  source("R/utils/parameters.R")
  
  library(furrr)
  library(tidyverse)
  library(raster)
  
  plan(multisession, workers = 5)
  
  step.name <- ifelse(time_step == 7, "7_days_", "daily_")
  param <- paramInit(time_step, param_to_modify = param_to_modify, 
                     percent_change = percent_change)
  
  key.nafo <- readRDS(paste0("data/polygons/", model_res, "km_keynafo.RDS"))
  
  # Process all years in parallel
  all_results <- future_map_dfr(my.years, function(my.year) {
    
    seascape <- readRDS(paste0("data/seascape/seascape_", model_res, "km_", 
                               step.name, my.year, ".RDS"))
    
    start_date <- ymd(paste0(my.year, "-01-01"))
    end_date <- ymd(paste0(my.year, "-12-30"))
    dates <- seq.Date(start_date, end_date, time_step)
    
    lyr <- length(seascape)
    
    # Process each date
    year_results <- future_map_dfr(seq_along(dates), function(t) {
      
      # Stack environmental layers
      seascape2 <- c(
        lapply(seascape[-c((lyr-1):lyr)], function(l) as(l[[t]], "Raster")),
        seascape[(lyr-1):lyr]
      ) %>% stack()
      
      # Convert all rasters to tibble (keep all cells with xy coordinates)
      sst_df <- as.data.frame(seascape2[[temp.var]], xy = TRUE) %>%
        rename(x = x, y = y, sst = names(seascape2[[temp.var]]))
      
      zoo_df <- as.data.frame(seascape2[[zoo.var]], xy = TRUE) %>%
        select(-x, -y) %>%
        rename(zooplankton = names(seascape2[[zoo.var]]))
      
      nafo_df <- as.data.frame(seascape2[["nafo"]], xy = TRUE) %>%
        select(-x, -y) %>%
        rename(nafo = names(seascape2[["nafo"]]))
      
      # Combine and calculate IR
      bind_cols(sst_df, zoo_df, nafo_df) %>%
        mutate(
          date = dates[t],
          year = my.year,
          X = zooplankton / 1000,
          A_sst = exp((-1 * param$Ea) / param$boltz * 
                        ((1 / sst) - (1 / param$Tref))),
          IR_g = param$Cmax * A_sst * 
            (X / (X + param$h)) * time_step,
            ) %>%
        select(date, year, x, y, nafo, sst, zooplankton, X,
               A_sst, IR_g) %>%  na.omit()
      
    }, .progress = TRUE)
    
    return(year_results)
    
  }, .progress = TRUE)
  
  return(all_results)
}

# ===== Summary statistics by zoo bins =====
plot_IR_zoo_summary_combined <- function(ir_tibble, 
                                         n_bins = 20,
                                         facet_by_primary = c("temp", "year", "month", "none"),
                                         facet_by_secondary = c("year", "month", "none")) {
  
  facet_by_primary <- match.arg(facet_by_primary)
  facet_by_secondary <- match.arg(facet_by_secondary)
  
  plot_data <- ir_tibble %>%
    filter(!is.na(IR_g), !is.na(zooplankton), !is.na(sst)) %>%
    mutate(
      month = month(date, label = TRUE),
      sst=sst-273,
      temp_category = case_when(
        sst < 7 ~ "< 7°C",
        sst >= 7 & sst < 16 ~ "7-16°C",
        sst >= 16 ~ ">= 16°C"
      ),
      temp_category = factor(temp_category, levels = c("< 7°C", "7-16°C", ">= 16°C")),
      zoo_bin = cut(X, breaks = n_bins)
    ) %>%
    group_by(year, month, temp_category, zoo_bin, h) %>%
    summarise(
      mean_IR = mean(IR_g, na.rm = TRUE),
      sd_IR = sd(IR_g, na.rm = TRUE),
      n = n(),
      zoo_mid = mean(X, na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- plot_data %>%
    ggplot(aes(x = zoo_mid, y = mean_IR)) +
    geom_point(aes(size = n, fill = sd_IR), alpha = 0.6, shape=21) +
    geom_errorbar(aes(ymin = mean_IR - sd_IR, ymax = mean_IR + sd_IR), 
                  width = 0.02, alpha = 0.5) +
    geom_smooth(se = TRUE, alpha = 0.2, aes(color = as.factor(h))) +
    scale_fill_viridis_c(name = "SD(IR)") +
    scale_color_viridis_d(name = "h", option="turbo") +
    scale_size_continuous(name = "N cells") +
    labs(
      title = "IR vs Zooplankton Density (Binned Summary)",
      x = "Zooplankton Density",
      y = "Mean Ingestion Rate (g/time_step)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  
  
  # Create formula for faceting
  if (facet_by_secondary == "none") {
    if (facet_by_primary == "temp") {
      p <- p + facet_wrap(~temp_category, scales = "free", ncol = 3)
    } else if (facet_by_primary == "year") {
      p <- p + facet_wrap(~year, scales = "free")
    } else if (facet_by_primary == "month") {
      p <- p + facet_wrap(~month, scales = "free")
    }
  } 
    # Two-way faceting
    if (facet_by_primary == "temp" && facet_by_secondary == "year") {
      p <- p + facet_grid(temp_category ~ year)
    } else if (facet_by_primary == "temp" && facet_by_secondary == "month") {
      p <- p + facet_grid(temp_category ~ month)
    } else if (facet_by_primary == "year" && facet_by_secondary == "temp") {
      p <- p + facet_grid(year ~ temp_category)
    } else if (facet_by_primary == "month" && facet_by_secondary == "temp") {
      p <- p + facet_grid(month ~ temp_category)
    }
  
  
  return(p)
}

