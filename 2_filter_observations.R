rm(list = ls())

library(rnoaa)
library(tidyverse)
library(lubridate)

source("0_climheat_funs.R")

project_dir <- "."

country_code <- "PL"
country_code_ghcn <- country_code

data_dir <- file.path(
    project_dir, 
    country_code)

# tolerable share of missed daily observations in a year
na_tol <- 0.3
n_days_in_year <- 365   

# read data --------------------------------------------------------------------
raw_data <- read.csv(
    file.path(data_dir, paste0(country_code, "_meteo_records.csv"))
)
national_ids <- unique(raw_data$id)

# standard names of GHCN data structure
minimax_cols <- c("tmin", "tmax")
# only those rows are of interest where not all observations are NAs
meaningful_data <- raw_data[
    # daily average can be restored having both minimax_cols available 
    # !(is.na(raw_data[, "tavg"])) | (rowSums(is.na(raw_data[, minimax_cols])) <= 0), 
    !(is.na(raw_data[, "tavg"])), 
] %>%
    mutate_at(vars(tavg, tmin, tmax),
    drop_unrealistic_tas    
    )

# filter-out representative stations -------------------------------------------
# a basic quality criterium is a number of missed data in a period (year in our case)
n_na_annual <- meaningful_data %>%
    mutate(
        year = year(ymd(date)),
        month = month(ymd(date)),
        day = day(ymd(date))
    ) %>%
    select(id, year, tavg) %>%
    group_by(id, year) %>%
    summarize_all(n_na)

# the requirement is linked with a need to have a possibly homogeneous dataset
quality_df <- n_na_annual %>%
    group_by(id) %>%
    summarize_all(max)

id_set_good_quality <- unique(
    quality_df[quality_df$tavg <= na_tol * n_days_in_year, "id"]
)

print("Stations providing 'good enough' data")
print(id_set_good_quality)

# read stations meta-data ------------------------------------------------------
t_1 <- Sys.time()
stations <- ghcnd_stations()
t_2 <- Sys.time()
print(t_2 - t_1)

stations_in_region <- stations %>%
    dplyr::filter(
        id %in% national_ids
    )

# look into the selected reference stations ------------------------------------
# show map
st_to_look_meta <- stations[
    (stations$id %in% unlist(id_set_good_quality)), ]
pdf(
    file.path(
        data_dir, 
        paste0(country_code, "_availabl_stations_map.pdf")
    )
)
show_stations(st_to_look_meta, annotate = TRUE)     
dev.off()

# show timeseries
data_df <- meaningful_data[, c("id", "date", "tavg")] %>%
    # dplyr::filter(id %in% "PLM00012160") %>%
    dplyr::filter(id %in% unlist(id_set_good_quality)) #%>%

data_ann_df <- data_df[, c("id", "date", "tavg")] %>%
    mutate(year = year(ymd(date))) %>%
    group_by(id, year) %>%
    summarize_all(mean)    

pl <- ggplot(data_df, aes(x = ymd(date), y = tavg/10, color = id, group_by = id))+
    geom_line() +
    facet_wrap(facets = vars(id))
ggsave(
    file.path(
        data_dir, 
        paste0(country_code, "_daily_ts.pdf")
    )
)

pl_ann <- ggplot(data_ann_df, 
    aes(x = year, y = tavg/10, color = id, group_by = id))+
    geom_line() +
    facet_wrap(facets = vars(id))
ggsave(
    file.path(
        data_dir, 
        paste0(country_code, "_ann_ts.pdf")
    )
)

# look for continous dataset ---------------------------------------------------
# it's perfectly possible to have some years completely missed
# that can be seen using differentiation of the "year" time-series
gaps_in_available_years <- lapply(
    FUN = function(x){
        unique(
            diff(unlist(n_na_annual[n_na_annual$id %in% x, "year"]), 1)
        )
    },
    X = unlist(id_set_good_quality)
)

# it's possible to select a dataset without gaps playing with the time boundaries
start_year <- 1976

n_na_annual_cut <- n_na_annual %>%
    dplyr::filter(
        year >= start_year
    )
gaps_in_available_years2 <- lapply(
    FUN = function(x){
        unique(
            diff(unlist(n_na_annual_cut[n_na_annual_cut$id %in% x, "year"]), 1)
        )
    },
    X = unlist(id_set_good_quality)
)
print(gaps_in_available_years2)


