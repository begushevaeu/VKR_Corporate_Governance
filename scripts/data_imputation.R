#### Загрузка пакетов ####

library(dplyr)
library(missForest)

#### Загрузка данных ####

data_day1 <- readRDS("data/day1_raw.rds")
data_day30 <- readRDS("data/day30_raw.rds")

#### Импутация пропусков для 1 дня ####

# Убираем целевую переменную
data_day1_imp_input <- dplyr::select(data_day1, -Underpricing_1)

# Приводим к data.frame для стабильной работы missForest
data_day1_imp_input <- as.data.frame(data_day1_imp_input)

# Импутируем пропуски
set.seed(123)
data_day1_imp <- missForest(data_day1_imp_input)$ximp

# Добавляем целевую переменную обратно
data_day1_imp$Underpricing_1 <- data_day1$Underpricing_1

#### Импутация пропусков для 30 дня ####

# Убираем строки, где пропущено значение зависимой переменной
data_day30 <- data_day30[!is.na(data_day30$Underpricing_30), ]

# Убираем целевую переменную
data_day30_imp_input <- dplyr::select(data_day30, -Underpricing_30)

# Приводим к data.frame
data_day30_imp_input <- as.data.frame(data_day30_imp_input)

# Импутируем пропуски
set.seed(123)
data_day30_imp <- missForest(data_day30_imp_input)$ximp

# Добавляем целевую переменную обратно
data_day30_imp$Underpricing_30 <- data_day30$Underpricing_30

#### Сохраняем промежуточные данные ####

saveRDS(data_day1_imp, file = "data/day1_imp.rds")
saveRDS(data_day30_imp, file = "data/day30_imp.rds")