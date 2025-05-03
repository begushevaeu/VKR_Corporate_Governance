library(dplyr)
library(stargazer)

#### === Загрузка данных === ####

data_day1_raw <- readRDS("data/regressions/raw_log/day1/data_ALL.rds")
data_day1_ready <- readRDS("data/day1_ready.rds")
data_day30_raw <- readRDS("data/regressions/raw_log/day30/data_ALL.rds")
data_day30_ready <- readRDS("data/day30_ready.rds")

#### === СОЗДАЁМ ПАПКУ ДЛЯ ВЫГРУЗКИ === ####

if (!dir.exists("output/logit_regressions_all")) {
  dir.create("output/logit_regressions_all")
}

#### === Вспомогательная функция для логитов === ####

run_logit_and_save <- function(data, formula, file, title) {
  model <- glm(formula, family = "binomial", data = data)
  stargazer(model,
            type = "html",
            out = file,
            title = title,
            digits = 2,
            intercept.bottom = FALSE,
            covariate.labels = NULL,
            dep.var.labels.include = FALSE)
}

#### === DAY 1 RAW === ####

## UNDP_5
run_logit_and_save(data_day1_raw,
                   UNDP_5 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day1_raw_log5.html",
                   "Логистическая регрессия (день 1, грязные данные, порог 5%)")

## UNDP_10
run_logit_and_save(data_day1_raw,
                   UNDP_10 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day1_raw_log10.html",
                   "Логистическая регрессия (день 1, грязные данные, порог 10%)")

#### === DAY 1 READY === ####

## UNDP_5
run_logit_and_save(data_day1_ready,
                   UNDP_5 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day1_ready_log5.html",
                   "Логистическая регрессия (день 1, чистые данные, порог 5%)")

## UNDP_10
run_logit_and_save(data_day1_ready,
                   UNDP_10 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day1_ready_log10.html",
                   "Логистическая регрессия (день 1, чистые данные, порог 10%)")

#### === DAY 30 RAW === ####

## UNDP_5
run_logit_and_save(data_day30_raw,
                   UNDP_5 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day30_raw_log5.html",
                   "Логистическая регрессия (день 30, грязные данные, порог 5%)")

## UNDP_10
run_logit_and_save(data_day30_raw,
                   UNDP_10 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day30_raw_log10.html",
                   "Логистическая регрессия (день 30, грязные данные, порог 10%)")

#### === DAY 30 READY === ####

## UNDP_5
run_logit_and_save(data_day30_ready,
                   UNDP_5 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day30_ready_log5.html",
                   "Логистическая регрессия (день 30, чистые данные, порог 5%)")

## UNDP_10
run_logit_and_save(data_day30_ready,
                   UNDP_10 ~ Mean_age_log + Board_size_log + Gender_variability + Independence + Independent_leader +
                     Current_exp_log + Past_exp_log + Number_of_committees + Foreign_directors + Total_assets_log +
                     Age_of_company_log + Year_hot + Tech_sector + ROA + Net_Debt_EBITDA + Current_ratio + EBITDA_Margin,
                   "output/logit_regressions_all/day30_ready_log10.html",
                   "Логистическая регрессия (день 30, чистые данные, порог 10%)")
