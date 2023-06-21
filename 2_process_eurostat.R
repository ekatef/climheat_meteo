# works after dataset is generated and loaded to a local disk with 1_load_observations.R
rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggplot2)
library(rnoaa)

country_code <- "PL"

data_dir <- file.path(".", 
    country_code )

data_id <- "winter"
run_id <- "version_23"
param_code <- "TAVG"

hdd_threshold <- 15
cdd_threshold <- 24

dt_hdd <- 18
dt_cdd <- 21

# time spans: the following work good for Poland
years_1 <- 1980:1989
years_2 <- 2012:2021

# quality parameters
share_missed <- 0
tol_missed_share <- 0.9
share_na_ann <- 0.7

# a bit of file management -----------------------------------------------------
work_dir <- file.path(data_dir, run_id)
dir.create(work_dir)
print(work_dir)

# functions --------------------------------------------------------------------
mean_na_rm <- function(x){
    mean(x, na.rm = TRUE)
}

sum_na_rm <- function(x){
    sum(x, na.rm = TRUE)
}

n_na <- function(x){
    sum(is.na(x))
}

# read data --------------------------------------------------------------------
meteo_data <- read.csv(
    file.path(data_dir, paste0(country_code, "_meteo_records.csv"))
)

ids_in_region <- unique(meteo_data$id)

# extract meta -----------------------------------------------------------------
t_1 <- Sys.time()
stations <- ghcnd_stations()
t_2 <- Sys.time()
print(t_2 - t_1)

stations_in_region <- stations %>%
    dplyr::filter(
        id %in% ids_in_region
    )  

st_meta <- unique(stations_in_region[, c("id", "latitude", "longitude", 
    "elevation", "name")]) 

# calculate HDDs/CDDs ----------------------------------------------------------
# filter by quality for years_1 and years_2 periods
data_df <- meteo_data %>%
    # mutate(var_name = as.numeric(!var_name)) %>%
    mutate(
        year = year(ymd(date)),
        month = month(ymd(date)),
        day = day(ymd(date)),
        hdd = ifelse(
                     ((tavg/10) <= hdd_threshold), 
                     (dt_hdd - (tavg/10)),
                     0
               ),
        cdd = ifelse(
                     ((tavg/10) >= cdd_threshold), 
                     ((tavg/10) - dt_cdd),
                     0
        )
    ) 

df_hdd_ann <- data_df %>%
    select(id, year, hdd) %>%
    group_by(id, year) %>%
    summarize_all(list(length, n_na, sum_na_rm)) %>%
    rename(n_obs_ann = fn1, n_na_ann = fn2, hdd_ann = fn3) %>%
    dplyr::filter(
                  n_obs_ann >= share_na_ann * 365,
                  n_na_ann <= share_na_ann * 365
    )

df_cdd_ann <- data_df %>%
    select(id, year, cdd) %>%
    group_by(id, year) %>%
    summarize_all(list(length, n_na, sum_na_rm)) %>%
    rename(n_obs_ann = fn1, n_na_ann = fn2, cdd_ann = fn3) %>%
    dplyr::filter(
                  n_obs_ann >= share_na_ann * 365,
                  n_na_ann <= share_na_ann * 365
    )


df_hdd_spartial <- left_join(df_hdd_ann, st_meta)
df_cdd_spartial <- left_join(df_cdd_ann, st_meta)

# process Eurostat data --------------------------------------------------------
benchmark_data <- read.table(
    file.path(data_dir, "pl_hdd_eurostat23.csv"),
    sep = "",
    dec = ".",
    header = FALSE
)

res_list <- vector(mode = "list", length = nrow(benchmark_data))

# the first row contain year values
for ( i in seq(from = 2, to = nrow(benchmark_data)) ){
    region <- benchmark_data[i, 1]
    data_row <- as.numeric(gsub(",", "", benchmark_data[i, -1]))
    time_row <- as.numeric(benchmark_data[1, -1])
    
    res_df <- data.frame(
        year = time_row,
        region = region,
        hdd_ann = data_row,
        stringsAsFactors = FALSE
    )

    res_list[[i]] <- res_df
}

eurostat_hdd_df <- bind_rows(res_list)

# Mazowieckie wojewodztwo seems to be lost
st_wojew_mapping <- read.table(
                text = 
                '"NA","Poland"
                "BALICE","Malopolskie"
                "NA","Slaskie"            
                "LAWICA","Wielkopolskie"
                "SZCZECIN","Zachodniopomorskie"
                "NA","Lubuskie"           
                "STRACHOWICE","Dolnoslaskie" 
                "NA","Opolskie"
                "SIEDLCE","Kujawsko-Pomorskie" 
                "ELBLAG-MILEJEWO","Warminsko-Mazurskie"
                "LEBA","Pomorskie"
                "OKECIE","L\303\263dzkie"     
                "NA","Swietokrzyskie"
                "WLODAWA","Lubelskie" 
                "NA","Podkarpackie"       
                "BIALYSTOK","Podlaskie"',             
                sep = ",",
                na.strings = "NA",
                stringsAsFactors = FALSE) %>%
    mutate_if(is.character, trimws) #%>%
colnames(st_wojew_mapping) <- c("name", "region")

eurostat_hdd_df2 <- left_join(eurostat_hdd_df, st_wojew_mapping) %>%
    mutate(name = trimws(name))

# Eurostat vs meteo-deriven HDDs intercomparison -------------------------------
eurostat_local <- eurostat_hdd_df2 %>%
    mutate(source = "eurostat") %>%
    select(year, hdd_ann, name, source)
tail(eurostat_local)

hdd_meteo_local <- df_hdd_spartial %>%
    mutate(source = "imgw") %>%
    select(year, hdd_ann, name, source)

compar_df <- bind_rows(eurostat_local, hdd_meteo_local) %>%
    filter(!is.na(name)) %>%
    filter(name != "NA")


pl <- compar_df %>%
    ggplot(
        aes(x = year, y = hdd_ann, group_by = source, color = source)
    )+
    geom_line()+
    facet_wrap(vars(name))
ggsave(
    file.path(work_dir, "hdd_valid_imgw_vs_eurostat.pdf")
)