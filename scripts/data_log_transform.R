#### Загрузка пакетов ####

library(dplyr)

#### Загрузка данных ####

data_day1_raw <- readRDS("data/day1_raw.rds")
data_day30_raw <- readRDS("data/day30_raw.rds")

data_day1_imp <- readRDS("data/day1_imp.rds")
data_day30_imp <- readRDS("data/day30_imp.rds")

#### Логарифмирование переменных ####

log_vars <- c("Total_assets", "Age_of_company", "Current_exp", "Past_exp", "Board_size", "Mean_age")

##### === Для грязных данных (до импутации) === #####

# Для 1 дня (грязные)
data_day1_log <- data_day1_raw

for (v in log_vars) {
  data_day1_log[[paste0(v, "_log")]] <- log(data_day1_log[[v]] + 1)
  data_day1_log[[v]] <- NULL
}

saveRDS(data_day1_log, file = "data/day1_log.rds")

# Для 30 дня (грязные)
data_day30_log <- data_day30_raw

for (v in log_vars) {
  data_day30_log[[paste0(v, "_log")]] <- log(data_day30_log[[v]] + 1)
  data_day30_log[[v]] <- NULL
}

saveRDS(data_day30_log, file = "data/day30_log.rds")


##### === Для чистых данных (после импутации) === #####

# Для 1 дня (чистые)
for (v in log_vars) {
  data_day1_imp[[paste0(v, "_log")]] <- log(data_day1_imp[[v]] + 1)
  data_day1_imp[[v]] <- NULL
}

saveRDS(data_day1_imp, file = "data/day1_ready.rds")

# Для 30 дня (чистые)
for (v in log_vars) {
  data_day30_imp[[paste0(v, "_log")]] <- log(data_day30_imp[[v]] + 1)
  data_day30_imp[[v]] <- NULL
}

saveRDS(data_day30_imp, file = "data/day30_ready.rds")