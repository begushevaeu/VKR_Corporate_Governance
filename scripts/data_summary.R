#### Загрузка пакетов ####

library(dplyr)
library(stargazer)

#### Загрузка данных ####

data_day1_raw <- readRDS("data/day1_raw.rds")
data_day30_raw <- readRDS("data/day30_raw.rds")

data_day1_imp <- readRDS("data/day1_imp.rds")
data_day30_imp <- readRDS("data/day30_imp.rds")

#### Описательная статистика для 1 дня - ДО импутации

data_day1_raw <- data.frame(data_day1_raw)
stargazer(data_day1_raw, type = "html", digits = 2, summary = TRUE, 
          out = "output/descriptive/summary_day1_raw.html")

#### Описательная статистика для 1 дня - ПОСЛЕ импутации

stargazer(data_day1_imp, type = "html", digits = 2, summary = TRUE, 
          out = "output/descriptive/summary_day1_imp.html")

#### Описательная статистика для 30 дня - ДО импутации

data_day30_raw <- data.frame(data_day30_raw)
stargazer(data_day30_raw, type = "html", digits = 2, summary = TRUE, 
          out = "output/descriptive/summary_day30_raw.html")

#### Описательная статистика для 30 дня - ПОСЛЕ импутации

stargazer(data_day30_imp, type = "html", digits = 2, summary = TRUE, 
          out = "output/descriptive/summary_day30_imp.html")