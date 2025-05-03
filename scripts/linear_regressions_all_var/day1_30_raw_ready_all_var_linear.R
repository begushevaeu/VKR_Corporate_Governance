library(dplyr)
library(stargazer)

#### === Загрузка данных === ####

data_day1_raw <- readRDS("data/regressions/raw_log/day1/data_ALL.rds")
data_day1_ready <- readRDS("data/day1_ready.rds")
data_day30_raw <- readRDS("data/regressions/raw_log/day30/data_ALL.rds")
data_day30_ready <- readRDS("data/day30_ready.rds")

#### === ЛИНЕЙНЫЕ РЕГРЕССИИ === ####

## Грязные данные - день 1
model_day1_raw <- lm(Underpricing_1 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                       Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log + 
                       Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                     data = data_day1_raw)

## Чистые данные - день 1
model_day1_ready <- lm(Underpricing_1 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                         Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log + 
                         Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                       data = data_day1_ready)

## Грязные данные - день 30
model_day30_raw <- lm(Underpricing_30 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                        Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log + 
                        Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                      data = data_day30_raw)

## Чистые данные - день 30
model_day30_ready <- lm(Underpricing_30 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                          Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log + 
                          Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                        data = data_day30_ready)

#### === СОХРАНЯЕМ === ####

if (!dir.exists("output/linear_regressions_all")) {
  dir.create("output/linear_regressions_all")
}

stargazer(model_day1_raw, type = "html", out = "output/linear_regressions_all/day1_raw.html", title = "Линейная регрессия (Все переменные, грязные данные, день 1)")
stargazer(model_day1_ready, type = "html", out = "output/linear_regressions_all/day1_ready.html", title = "Линейная регрессия (Все переменные, чистые данные, день 1)")
stargazer(model_day30_raw, type = "html", out = "output/linear_regressions_all/day30_raw.html", title = "Линейная регрессия (Все переменные, грязные данные, день 30)")
stargazer(model_day30_ready, type = "html", out = "output/linear_regressions_all/day30_ready.html", title = "Линейная регрессия (Все переменные, чистые данные, день 30)")
