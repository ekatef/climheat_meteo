rm(list = ls())

library(rnoaa)
library(tidyverse)
library(lubridate)

project_dir <- "."

country_code <- "PL"
country_code_ghcn <- country_code


data_dir <- file.path(
    project_dir, 
    country_code)


# read meta-data ---------------------------------------------------------------

t_1 <- Sys.time()
stations <- ghcnd_stations()
t_2 <- Sys.time()
cat(t_2 - t_1, "seconds \n\r")

stations_in_region <- stations[grepl(country_code_ghcn, stations$id), ] 
ids_in_region <- unique(stations_in_region$id)
ids_national <- ids_in_region[grepl(paste0("^", country_code_ghcn), ids_in_region)] 

cat("ids_national\n\r", ids_national, "\n\r")

meta_national <- stations %>%
    dplyr::filter(id %in% ids_national)

cat("meta_national\n\r")
print(meta_national)

# load data --------------------------------------------------------------------

extracted_data <- meteo_tidy_ghcnd(stationid = ids_national)
write.csv(
    extracted_data, 
    file.path(data_dir, paste0(country_code, "_meteo_records.csv")),
    row.names = FALSE
)  
