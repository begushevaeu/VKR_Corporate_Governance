#### Загрузка пакетов ####

library(dplyr)
library(stargazer)

#### Загрузка данных ####

data_day1_ready <- readRDS("data/day1_ready.rds")
data_day30_ready <- readRDS("data/day30_ready.rds")

#### Таблица с количеством недооценок для 1 дня ####

limits <- c(0, 0.05, 0.1, 0.15, 0.2)

result_day1 <- data.frame(Limit = numeric(),
                          Underpricing_YES = numeric(),
                          Underpricing_NO = numeric())

for (limit in limits) {
  data_day1_ready$UNDP1 <- ifelse(data_day1_ready$Underpricing_1 < limit, 0, 1)
  count_1 <- round((sum(data_day1_ready$UNDP1 == 1) / nrow(data_day1_ready)) * 100, 2)
  count_0 <- round((sum(data_day1_ready$UNDP1 == 0) / nrow(data_day1_ready)) * 100, 2)
  result_day1 <- rbind(result_day1, data.frame(Limit = limit * 100,
                                               Underpricing_YES = count_1,
                                               Underpricing_NO = count_0))
}

#### Сохраняем таблицу в HTML для 1 дня ####

stargazer(result_day1, type = "html", summary = FALSE, digits = 2,
          out = "output/descriptive/undp1_distribution_day1.html")

#### Таблица с количеством недооценок для 30 дня ####

result_day30 <- data.frame(Limit = numeric(),
                           Underpricing_YES = numeric(),
                           Underpricing_NO = numeric())

for (limit in limits) {
  data_day30_ready$UNDP30 <- ifelse(data_day30_ready$Underpricing_30 < limit, 0, 1)
  count_1 <- round((sum(data_day30_ready$UNDP30 == 1) / nrow(data_day30_ready)) * 100, 2)
  count_0 <- round((sum(data_day30_ready$UNDP30 == 0) / nrow(data_day30_ready)) * 100, 2)
  result_day30 <- rbind(result_day30, data.frame(Limit = limit * 100,
                                                 Underpricing_YES = count_1,
                                                 Underpricing_NO = count_0))
}

#### Сохраняем таблицу в HTML для 30 дня ####

stargazer(result_day30, type = "html", summary = FALSE, digits = 2,
          out = "output/descriptive/undp1_distribution_day30.html")