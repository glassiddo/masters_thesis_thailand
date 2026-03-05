simplifcation <- 0.001

create_unit_plot <- function(Y1, Y0, weights, unit_id, names_key) {
  actual <- Y1[, as.character(unit_id)]
  synthetic <- Y0 %*% weights[, as.character(unit_id)]

  plot_df <- data.frame(
    year = 2003 + n_begin + (1:length(actual)) - 1,
    actual = actual,
    synthetic = synthetic
  )

  unit_name <- names_key %>%
    filter(adm1_id == as.character(unit_id)) %>%
    pull(ADM1_EN)

  ggplot(plot_df, aes(x = year)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 1.05) +
    geom_line(aes(y = synthetic, color = "Synthetic"), linewidth = 1.05) +
    geom_vline(xintercept = 2013, linetype = "dashed", color = "black", linewidth = 1.05) +
    geom_vline(xintercept = 2017, linetype = "dashed", color = "black", linewidth = 1.05) +
    scale_color_manual(values = c("Actual" = "#2E86AB", "Synthetic" = "#A23B72")) +
    labs(title = unit_name) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      #legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank()
    )
}

create_single_synth_weights_map <- function(treated_unit_id, weights_matrix,
                                            current_period_name, names_key) {
  # Get the weights for the specific treated unit
  unit_weights <- weights_matrix[, as.character(treated_unit_id), drop = FALSE]

  # Map the units to names
  map_data <- names_key %>%
    left_join(
      as.data.frame(unit_weights) %>%
        rownames_to_column(var = "adm1_id") %>%
        rename(weight = !!as.character(treated_unit_id)),
      by = "adm1_id"
    ) %>%
    mutate(
      weight = ifelse(is.na(weight), 0, weight), # replace NAs with 0
      weight_category = case_when(
        adm1_id == as.character(treated_unit_id) ~ "Treated Unit",
        adm1_id %in% treated_units &
          adm1_id != as.character(treated_unit_id) ~ "Other treated",
        weight == 0 ~ "No Weight",
        weight <= 0.05 ~ "0.0 - 0.05",
        weight <= 0.1 ~ "0.05 - 0.1",
        weight <= 0.2 ~ "0.1 - 0.2",
        weight <= 0.3 ~ "0.2 - 0.3",
        weight <= 0.5 ~ "0.3 - 0.5",
        TRUE ~ "0.5+"
      )
    )

  # Define colors
  colors <- c(
    "Treated Unit" = "#2d3748",       # Dark grey/black for treated
    "Other treated" = "grey",      # light grey for other treated
    "0.0 - 0.05" = "#fee0d2",        # Lighter red
    "0.05 - 0.1" = "#fcbba1",
    "0.1 - 0.2" = "#fc9272",
    "0.2 - 0.3" = "#fb6a4a",
    "0.3 - 0.5" = "#ef3b2c",
    "0.5+" = "#cb181d"                # Darkest red
  )

  # Order categories for legend
  weight_category_levels <- c("Treated Unit", "0.5+", "0.3 - 0.5", "0.2 - 0.3",
                              "0.1 - 0.2", "0.05 - 0.1", "0.0 - 0.05", "Other treated")
  map_data$weight_category <- factor(map_data$weight_category, levels = weight_category_levels)

  treated_unit_name <- names_key %>%
    filter(adm1_id == as.character(treated_unit_id)) %>%
    pull(ADM1_EN)

  map_data_simp = map_data %>%
    rmapshaper::ms_simplify(keep = 0.001) %>%
    st_make_valid() %>%
    mutate(centroid = st_centroid(geometry),
           coords = st_coordinates(centroid),
           lat = coords[,"Y"], lon = coords [, "X"]) %>%
    #filter(lat > 13, lat < 25, lon < 103) %>%
    select(-centroid, -coords)

  ggplot(map_data_simp) +
    geom_sf(data = map_data_simp, aes(fill = weight_category),
            size = 0.3, show.legend=TRUE) +
    geom_sf(data = all_units_countries, aes(color = factor(ADM0_EN)), fill = NA, linewidth = 0.5, show.legend = TRUE) +
    scale_color_manual(
      name = "Country borders",
      values = c("Thailand" = "#000000", "Myanmar" = "#009E73", "Laos" = "#0072B2")) + 
    scale_fill_manual( 
      name = "Weight in SC",
      values = colors,
      breaks = weight_category_levels,  # Explicitly specify breaks
      labels = weight_category_levels,  # Explicitly specify labels
      drop = FALSE                      # Don't drop unused levels
    ) +
    theme_minimal() +
    theme(
      plot.title = #element_blank(),
        element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",            
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.box.just = "center",          # Center the legend box
      legend.justification = "center",     # Center individual legends
      legend.margin = margin(t = 0, r = 5, b = 0, l = 5),  # Add some spacing between legends
      panel.background = element_rect(fill = "white", color = "white")
    ) +
    labs(
      title = paste(treated_unit_name)
      #subtitle = paste("Period:", current_period_name),
    ) + 
    guides(
      fill = guide_legend(override.aes = list(color = NA)),  # Remove borders from fill legend
      color = guide_legend(override.aes = list(fill = c("#0072B2", "#009E73", "#000000"), 
                                               linewidth = 0.1))
    )
}

create_weights_table_single_unit <-
  function(analysis_results, treated_unit_id, names_key, w_threshold,
           category_filter = NULL, filter_type = c("outcome", "period")) {
    # Extract weights for the specific treated unit across all periods
    weights_by_period <- list()

    for (result_key in names(analysis_results$detailed_results)) {
      result <- analysis_results$detailed_results[[result_key]]
      period_name <- result$period
      outcome_name <- result$dependent_var
      weights_matrix <- result$weights

      key <- paste0(outcome_name, "_", period_name)
      unit_weights <- weights_matrix[, as.character(treated_unit_id)]
      weights_by_period[[key]] <- unit_weights
    }

    # Get all donor units that appear in any period
    all_donor_units <- unique(unlist(lapply(weights_by_period, names)))

    # Create the base table with donor unit info
    result_table <- data.frame(
      adm1_id = all_donor_units,
      stringsAsFactors = FALSE
    )

    # Add donor unit names
    result_table <- result_table %>%
      left_join(names_key %>% st_drop_geometry(), by = "adm1_id") %>%
      select(adm1_id, donor_unit_name = ADM1_EN) %>%
      arrange(donor_unit_name)

    # Add columns for each period with numeric weights
    for (period_key in names(weights_by_period)) {
      period_weights <- weights_by_period[[period_key]]

      # Add this period's weights as a column
      result_table[[period_key]] <- sapply(result_table$adm1_id, function(unit_id) {
        return(round(period_weights[unit_id], 4))  # Round to 4 decimal places
      })
    }

    # Filter to only include rows where at least one column has weight > threshold
    weight_columns <- names(weights_by_period)
    result_table <- result_table %>%
      filter(if_any(all_of(weight_columns), ~ .x > w_threshold))

    ## apply filter type
    if (filter_type == "outcome") {
      # Filter columns that start with the outcome category
      selected_cols <- grep(paste0("^", category_filter, "_"), names(result_table), value = TRUE)
    } else if (filter_type == "period") {
      # Filter columns that end with the period category
      selected_cols <- grep(paste0("_", category_filter, "$"), names(result_table), value = TRUE)
    }

    # Keep only rows with at least one weight > threshold in the selected columns
    result_table <- result_table %>%
      filter(if_any(all_of(selected_cols), ~ .x > w_threshold)) %>%
      select(donor_unit_name, all_of(selected_cols))

    return(result_table)
  }


all_units_countries <- read_sf("2. intermediate/units/adm1_units_detailed.shp") %>% 
  mutate(centroid = st_centroid(geometry),
         coords = st_coordinates(centroid),
         lat = coords[,"Y"], lon = coords [, "X"]) %>%
  #filter(lat > 13, lat < 25, lon < 103) %>% 
  select(-centroid) %>% 
  rmapshaper::ms_simplify(keep = 0.01) %>% 
  st_make_valid() %>% 
  group_by(ADM0_EN) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_make_valid() 
  


#### run all -----
create_plots <- function(results, northern_regions_ids, names_key) {
  plot_list <- list()
  weights_maps_list <- list()
  unit_plots_list <- list()
  
  for (result_key in names(results)) {
    result <- results[[result_key]]
    
    # Effect plot - average for treated, individual for control
    treated_avg <- result$mspe_data %>%
      filter(is_treated) %>%
      summarise(avg_ratio = mean(ratio_strong, na.rm = TRUE)) %>%
      mutate(unit = "Average ratio for the treated", is_treated = TRUE)

    control_individual <- result$mspe_data %>%
      filter(!is_treated) %>%
      select(unit, ratio_strong, is_treated) %>%
      rename(avg_ratio = ratio_strong)

    # Combine data
    plot_data <- bind_rows(treated_avg, control_individual)

    p <- ggplot(plot_data, aes(x = reorder(unit, avg_ratio), y = avg_ratio,
                               fill = ifelse(is_treated, "Average ratio for the treated", "Control units"))) +
      geom_col(width = 0.8) +  # Make bars slightly thicker
      geom_col(data = filter(plot_data, is_treated),
               aes(x = reorder(unit, avg_ratio), y = avg_ratio),
               fill = "red", width = 1.5) +  # Make treated bar wider and more prominent
      coord_flip() +
      scale_fill_manual(values = c("Average ratio for the treated" = "red", "Control units" = "lightblue")) +
      labs(title = paste("MSPE ratios post-pre treatment, 2017-2020, for", result$period)) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11),  # Slightly larger x-axis numbers
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Bigger legend text
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Bold and centered title
      )
    plot_list[[result_key]] <- p

    treated_ids <- colnames(result$weights)
    individual_maps <- list()
    
    for (treated_id in treated_ids) {
      map_plot <- create_single_synth_weights_map(
        treated_unit_id = as.numeric(treated_id),
        weights_matrix = result$weights,
        current_period_name = result$period,
        names_key = names_key
      )
      
      # Get the treated unit name for the title
      treated_unit_name <- names_key %>%
        filter(adm1_id == treated_id) %>%
        pull(ADM1_EN)
      
      individual_maps[[treated_id]] <- map_plot
    }
    
    combined_plot <- wrap_plots(individual_maps) + 
      plot_layout(guides = "collect")  +
      plot_annotation(
        theme = theme(
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "center",
          legend.justification = "center"
        )
      )
    # Store the final combined plot instead of individual ones
    weights_maps_list[[result_key]] <- combined_plot
    
    # Unit plots - ADD PERIOD HERE
    plot_list_units <- lapply(northern_regions_ids, function(id) {
      create_unit_plot(result$Y1, result$Y0, result$weights, id, names_key)
    })

    legend_plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
      geom_line(aes(color = "Actual"), linewidth = 1.25) +
      geom_line(aes(color = "Synthetic"), linewidth = 1.25) +
      scale_color_manual(values = c("Actual" = "#2E86AB", "Synthetic" = "#A23B72")) +
      guides(color = guide_legend(override.aes = list(linewidth = 2.5))) +  # bigger color lines in legend
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.width = unit(2, "cm")
      )

    combined_plot <- ggarrange(
      plotlist = plot_list_units,
      ncol = 3, nrow = 3,
      common.legend = TRUE,
      legend.grob = get_legend(legend_plot),
      legend = "bottom"
    )

    # Add period information as both title and subtitle
    final_plot <- ggdraw(combined_plot) +
      draw_label(
        paste("Period:", result$period),
        x = 0.5, y = 1.02,
        hjust = 0.5, vjust = 0,
        size = 18, fontface = "bold", color = "#333333"
      ) +
      theme(plot.margin = margin(1.2, 0.5, 0.5, 0.5, unit = "cm"))  # increased top margin for both labels

    unit_plots_list[[result_key]] <- final_plot
  }
  
  list(
    effect_plots = plot_list,
    weights_maps = weights_maps_list,
    unit_plots = unit_plots_list
  )
}
all_plots <- create_plots(
  analysis_results$detailed_results,
  northern_regions_ids,
  names_key
)
all_plots$weights_maps$`total_fires_Jan-May`
