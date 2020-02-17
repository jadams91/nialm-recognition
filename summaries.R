
data_summary <- function(.data) {
  .hourly <- .data %>% group_by(phase, hour = hour(time)) %>%
    summarise(mcurr = mean(current), 
              mhar3 = mean(harmonic_3), 
              mhar5 = mean(harmonic_5), 
              mhar7 = mean(harmonic_7), 
              mhar9 = mean(harmonic_9))
  
  current_plot <- ggplot(.hourly) + 
    geom_col(mapping = aes(x = hour, y = mcurr, fill = phase), 
             position = "dodge")
  harmon3_plot <- ggplot(.hourly) + 
    geom_col(mapping = aes(x = hour, y = mhar3, fill = phase), 
             position = "dodge")
  harmon5_plot <- ggplot(.hourly) + 
    geom_col(mapping = aes(x = hour, y = mhar5, fill = phase), 
             position = "dodge")
  harmon7_plot <- ggplot(.hourly) + 
    geom_col(mapping = aes(x = hour, y = mhar7, fill = phase), 
             position = "dodge")
  harmon9_plot <- ggplot(.hourly) + 
    geom_col(mapping = aes(x = hour, y = mhar9, fill = phase), 
             position = "dodge")
  harmon_plot <- grid.arrange(harmon3_plot, harmon5_plot, 
                              harmon7_plot, harmon9_plot)
  
  .difs <- .data %>% group_by(phase, hour = hour(time)) %>%
    summarise(dcurr = current %>% dif %>% abs %>%  mean, 
              dhar3 = harmonic_3 %>% dif %>% abs %>% mean, 
              dhar5 = harmonic_5 %>% dif %>% abs %>% mean, 
              dhar7 = harmonic_7 %>% dif %>% abs %>% mean, 
              dhar9 = harmonic_9 %>% dif %>% abs %>% mean)
  
  dcurrent_plot <- ggplot(.difs) + 
    geom_col(mapping = aes(x = hour, y = dcurr, fill = phase), 
             position = "dodge")
  dharmon3_plot <- ggplot(.difs) + 
    geom_col(mapping = aes(x = hour, y = dhar3, fill = phase), 
             position = "dodge")
  dharmon5_plot <- ggplot(.difs) + 
    geom_col(mapping = aes(x = hour, y = dhar5, fill = phase), 
             position = "dodge")
  dharmon7_plot <- ggplot(.difs) + 
    geom_col(mapping = aes(x = hour, y = dhar7, fill = phase), 
             position = "dodge")
  dharmon9_plot <- ggplot(.difs) + 
    geom_col(mapping = aes(x = hour, y = dhar9, fill = phase), 
             position = "dodge")
  dharmon_plot <- grid.arrange(dharmon3_plot, dharmon5_plot, 
                               dharmon7_plot, harmon9_plot)
  
  
  # s <- seq(0, 20, 0.1)
  # .dens <- .data %>% group_by(phase) %>% 
  #   mutate_if(is.numeric, ~abs(dif(.))) %>% 
  #   group_map(~ tibble(
  #     s         = s, 
  #     rocurrent = 1 - ecdf(.$current)(s), 
  #     roharmon3 = 1 - ecdf(.$harmonic_3)(s), 
  #     roharmon5 = 1 - ecdf(.$harmonic_5)(s), 
  #     roharmon7 = 1 - ecdf(.$harmonic_7)(s), 
  #     roharmon9 = 1 - ecdf(.$harmonic_9)(s))) %>% 
  #   tibble(phase = as.factor(1:3), data = .) %>% 
  #   unnest
  # 
  # current_dens <- ggplot(.dens) + 
  #   geom_histogram(mapping = aes(x = current, fill = phase), 
  #                  position = position_dodge(width = 0.09), binwidth = 0.1) + 
  #   scale_y_log10()
  # 
  # ggplot(.dens) + 
  #   geom_line(aes(x = s, y = rocurrent, color = phase)) + scale_y_log10()
  # ggplot(.dens) + 
  #   geom_line(aes(x = s, y = roharmon3, color = phase)) + scale_y_log10()
  # ggplot(.dens) + 
  #   geom_line(aes(x = s, y = roharmon5, color = phase)) + scale_y_log10()
  # ggplot(.dens) + 
  #   geom_line(aes(x = s, y = roharmon7, color = phase)) + scale_y_log10()
  # ggplot(.dens) + 
  #   geom_line(aes(x = s, y = roharmon9, color = phase)) + scale_y_log10()
  
  
  list(
    plots = list(
      mean_hourly_current = current_plot, 
      mean_hourly_harmonics = harmon_plot, 
      mean_hourly_current_difference = dcurrent_plot, 
      mean_hourly_harmonics_difference = dharmon_plot
    )
  )
}


#### -------------------------------- DATA ------------------------------------

summarise_data <- function(.data, plot_cols, 
                           time_col = "time", phase_col = "phase") {
  
  .data %<>% group_by(!!parse_expr(phase_col))
  summ_hour <- .data %>% 
    mutate(thour = hour(!!parse_expr(time_col))) %>% 
    group_by(!!parse_expr(phase_col), thour) %>% 
    summarise_if(is.numeric, mean)
  summ_date <- .data %>% 
    mutate(tdate = date(!!parse_expr(time_col))) %>% 
    group_by(!!parse_expr(phase_col), tdate) %>% 
    summarise_if(is.numeric, mean)
  
  for (col in plot_cols) {
    ggplot(data = summ_hour) + 
      geom_line(mapping = aes(x = thour, y = !!parse_expr(col), 
                               colour = !!parse_expr(phase_col))) + 
      labs(x = "Hour of the Day", y = col, colour = "Phase", 
           title = paste0("Hourly average of ", col))
    ggsave(paste0("summary_data/hourly_", col, ".png"), 
           width = 16, height = 9, units = "in")
    
    ggplot(data = summ_date) + 
      geom_line(mapping = aes(x = tdate, y = !!parse_expr(col), 
                               colour = !!parse_expr(phase_col))) + 
      labs(x = "Date", y = col, colour = "Phase", 
           title = paste0("Daily average of ", col))
    ggsave(paste0("summary_data/daily_", col, ".png"), 
           width = 16, height = 9, units = "in")
    
    summ_phase <- .data %>% 
      summarise(mean = mean(!!parse_expr(col)), 
                min  = min(!!parse_expr(col)), 
                max  = max(!!parse_expr(col)))
    write_csv(summ_phase, path = paste0("summary_data/table_", col, ".csv"))
  }
}


#### -------------------------------- EVENTS -----------------------------------

summarise_events <- function(.data, plot_cols, summary_date = NA, 
                             time_col = "time", phase_col = "phase", 
                             event_col = "event") {
  
  events <- filter(.data, !!parse_expr(event_col))
  span <- events[[time_col]] %>% 
    (function(x) max(x) - min(x)) %>% 
    as.double(units = "days")
  
  summ_hour <- events %>% 
    mutate(thour = hour(!!parse_expr(time_col))) %>% 
    group_by(!!parse_expr(phase_col), thour) %>% 
    summarise(count = length(!!parse_expr(event_col)) / span)
  
  ggplot(summ_hour) + 
    geom_line(mapping = aes(x = thour, y = count, colour = phase)) + 
    labs(x = "Hour of the Day", y = "Average Event Count", colour = "Phase", 
         title = paste0("Hourly average number of events"))
  ggsave(paste0("summary_events/hourly_event_count", ".png"), 
         width = 16, height = 9, units = "in")
    
  summ_date <- events %>% 
    mutate(tdate = date(!!parse_expr(time_col))) %>% 
    group_by(!!parse_expr(phase_col), tdate) %>% 
    summarise(count = length(!!parse_expr(event_col)))
  
  ggplot(summ_date) + 
    geom_line(mapping = aes(x = tdate, y = count, colour = phase)) + 
    labs(x = "Date", y = "Average Event Count", colour = "Phase", 
         title = paste0("Daily average number of events"))
  ggsave(paste0("summary_events/daily_event_count", ".png"), 
         width = 16, height = 9, units = "in")
  
  if (missing(summary_date)) {
    summary_date <- summ_date %>% 
      group_by(tdate) %>% 
      summarise(s = sum(count)) %>% 
      filter(s == median(s)) %$% 
      tdate %>% 
      `[[` (1)
  }
  
  day_data <- filter(.data, date(!!parse_expr(time_col)) == summary_date)
  
  for (col in plot_cols) {
    ggplot(data = day_data) + 
      geom_line(mapping = aes_string(x = time_col, y = col, 
                                     colour = phase_col)) + 
      geom_point(data = filter(day_data, !!parse_expr(event_col)), 
                 mapping = aes_string(x = time_col, y = col, 
                                      shape = phase_col), size = 2) + 
      labs(x = "Time", y = paste0("Data: ", col), colour = "Phase", 
           shape = "Phase", title = paste0("Events on the ", col, 
                                           " data, day: ", summary_date))
    ggsave(paste0("summary_events/", summary_date, "_events_", col, ".png"), 
           width = 16, height = 9, units = "in")
  }
  
  summ_event <- events %>% 
    arrange(!!parse_expr(time_col)) %>% 
    select(event_time = time_col, event_phase = phase_col)
  write_csv(summ_event, path = "summary_events/events.csv")
}


#### ------------------------------ PROCESSING ---------------------------------

summarise_processing <- function(.data, plot_cols, summary_date = NA, 
                                 time_col = "time", phase_col = "phase", 
                                 event_col = "event", 
                                 pca_cols = c("pc1", "pc2")) {
  
  ggplot(data = .data) + 
    geom_jitter(mapping = aes_string(x = pca_cols[[1]], y = pca_cols[[2]], 
                                     colour = phase_col)) + 
    labs(x = "1st Principal Component", y = "2nd Principal Component", 
         colour = "Phase", 
         title = "Principal component analysis of all detected events")
  ggsave(paste0("summary_processing/pca.png"), 
         width = 16, height = 9, units = "in")
}


#### ------------------------------ CLUSTERING ---------------------------------

summarise_clustering <- function(.data, n = 16, plot_cols, summary_date = NA, 
                                 time_col = "time", phase_col = "phase", 
                                 event_col = "event", 
                                 pca_cols = c("pc1", "pc2"), 
                                 class_col = "class") {
  
  ggplot(data = .data) + 
    geom_jitter(mapping = aes_string(x = pca_cols[[1]], y = pca_cols[[2]], 
                                     shape = phase_col, colour = class_col), 
                size = 1) + 
    labs(x = "1st Principal Component", y = "2nd Principal Component", 
         shape = "Phase",
         colour = "Class", 
         title = "PCA distribution of detected classes")
  ggsave(paste0("summary_clustering/class_distribution.png"), 
         width = 16, height = 9, units = "in")
  
  ggplot(data = .data) + 
    geom_bar(mapping = aes_string(x = class_col, fill = phase_col)) + 
    labs(x = "Time of the Day", y = "Density")
  ggsave(paste0("summary_clustering/class_hourly_density.png"), 
         width = 16, height = 9, units = "in")
  
  ggplot(data = .data) + 
    geom_density(mapping = aes(x = hour(!!parse_expr(time_col)))) + 
    facet_wrap(as.formula(paste("~", class_col))) + 
    labs(x = "Time of the Day", y = "Density")
  ggsave(paste0("summary_clustering/class_hourly_density_seperate.png"), 
         width = 16, height = 9, units = "in")
  
  summ_class <- .data %>% 
    select(time_col, phase_col, class_col) %>% 
    arrange(!!parse_expr(time_col))
  write_csv(summ_class, path = "summary_clustering/classes.csv")
  
}
