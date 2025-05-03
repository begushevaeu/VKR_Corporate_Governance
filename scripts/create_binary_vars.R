#### Загрузка пакетов ####

library(dplyr)

#### Загрузка данных ####

data_day1_raw <- readRDS("data/day1_raw.rds")
data_day30_raw <- readRDS("data/day30_raw.rds")

data_day1_imp <- readRDS("data/day1_imp.rds")
data_day30_imp <- readRDS("data/day30_imp.rds")

#### Функция для создания бинарных переменных ####

create_undp_vars <- function(data, var_name) {
  data$UNDP_5 <- ifelse(data[[var_name]] < 0.05, 0, 1)
  data$UNDP_10 <- ifelse(data[[var_name]] < 0.10, 0, 1)
  return(data)
}

#### Добавляем переменные в данные ДО импутации ####

data_day1_raw <- create_undp_vars(data_day1_raw, "Underpricing_1")
data_day30_raw <- create_undp_vars(data_day30_raw, "Underpricing_30")

#### Добавляем переменные в данные ПОСЛЕ импутации ####

data_day1_imp <- create_undp_vars(data_day1_imp, "Underpricing_1")
data_day30_imp <- create_undp_vars(data_day30_imp, "Underpricing_30")

#### Сохраняем обратно (перезаписываем) ####

saveRDS(data_day1_raw, "data/day1_raw.rds")
saveRDS(data_day30_raw, "data/day30_raw.rds")

saveRDS(data_day1_imp, "data/day1_imp.rds")
saveRDS(data_day30_imp, "data/day30_imp.rds")