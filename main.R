library(gridExtra)
library(lubridate)
library(magrittr)
library(rlang)
library(tidyverse)

setwd("/home/sebastian/Documents/2019/clustering")
source("config.R")
source("helpers.R")
source("processing.R")
source("summaries.R")

args <- commandArgs(trailingOnly = TRUE)
print(args)
if (length(args) != 1) {
  stop(
    "A path to the data file should be supplied 
    to the script as a single argument", 
    call. = FALSE
  )
}


for (dr in c("summary_data", "summary_events", 
             "summary_processing", "summary_clustering"))
  if (!dir.exists(dr)) dir.create(dr)
  

path_to_data <- "/home/sebastian/Documents/2019/databases/home/dump800.csv"
# path_to_data <- args[[1]]

raw_data <- get_data(path_to_data)
summarise_data(raw_data, plot_cols = DATA_COLS)

evt_data <- detect_events(raw_data)
summarise_events(evt_data, plot_cols = DATA_COLS)
rm(raw_data)

prc_data <- process_data(evt_data, data_cols = DATA_COLS)
summarise_processing(prc_data, plot_cols = DATA_COLS)
 
clu_data <- cluster_data(prc_data, data_cols = DATA_COLS)
summarise_clustering(clu_data, plot_cols = DATA_COLS)
