
get_data <- function(.path) {
  raw <- read_csv(.path, na = "\\N", locale = locale(tz = Sys.timezone()), 
                  col_types = cols(
                    .default = col_double(),
                    `2019-09-04 00:00:00.537` = col_datetime(format = "")))
  colnames(raw) <- c("n", "v1", "v2", "v3", "time", "i1", "i2", "i3", 
                     paste0("h1_", 1:10), 
                     paste0("h2_", 1:10), 
                     paste0("h3_", 1:10), 
                     "vang1", "vang2", "vang3", 
                     "iang1", "iang2", "iang3", 
                     "ap1", "ap2", "ap3", 
                     "rp1", "rp2", "rp3")
  
  data <- tibble(
    time = raw$time %>% rep(times = 3), 
    phase = 1:3 %>% rep(each = length(raw$time)) %>% as_factor, 
    voltage = c(raw$v1, raw$v2, raw$v3), 
    current = c(raw$i1, raw$i2, raw$i3), 
    harmonic_3 = c(raw$h1_3, raw$h2_3, raw$h3_3), 
    harmonic_5 = c(raw$h1_5, raw$h2_5, raw$h3_5), 
    harmonic_7 = c(raw$h1_7, raw$h2_7, raw$h3_7), 
    harmonic_9 = c(raw$h1_9, raw$h2_9, raw$h3_9), 
    voltage_angle = c(raw$vang1, raw$vang2, raw$vang3), 
    current_angle = c(raw$iang1, raw$iang2, raw$iang3)
  )
  
  data %>% arrange(time, phase)
}

detect_events <- function(.data, ...) {
  evts <- .data %>% 
    group_by(phase) %>% 
    mutate(event = find_events(current, ...))
}


process_data <- function(.data, k = 120, data_cols, time_col = "time", 
                         phase_col = "phase", event_col = "event", ...) {
  proc <- .data %>% 
    select(phase_col, time_col, data_cols, event) %>% 
    mutate_if(is.numeric, ~ dif(.) %>% soften(k = k)) %>% 
    filter(event) %>% 
    mutate_if(is.numeric, divide_by_sd)
  
  pmat <- proc %>% 
    ungroup %>%
    select(data_cols) %>% 
    as.matrix
  
  pca <- prcomp(pmat)
  proc <- proc %>%
    ungroup %>% 
    mutate(pc1 = pca$x[, 1], pc2 = pca$x[, 2]) %>% 
    group_by(!!parse_expr(phase_col))
  
  proc
}


cluster_data <- function(.data, n = 16, data_cols, time_col = "time", 
                         phase_col = "phase", event_col = "event", 
                         pca_cols = c("pc1", "pc2"), ...) {
  
  pmat <- .data %>% 
    ungroup %>%
    select(data_cols) %>% 
    as.matrix
  
  km <- kmeans(pmat, centers = n, nstart = 20)
  write_csv(as_tibble(km$centers), "summary_clustering/centers.csv")
  
  clus <- .data %>% 
    ungroup %>% 
    mutate(class = as.factor(km$cluster)) %>% 
    group_by(!!parse_expr(phase_col), class)
  
  clus
}
