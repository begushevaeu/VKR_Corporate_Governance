#### Загрузка пакетов ####

library(dplyr)

#### Загрузка данных ####

data_day1_log <- readRDS("data/day1_log.rds")
data_day30_log <- readRDS("data/day30_log.rds")

#### Создание папок для сохранения ####

dir.create("output/regressions/raw_log/day1", recursive = TRUE, showWarnings = FALSE)
dir.create("output/regressions/raw_log/day30", recursive = TRUE, showWarnings = FALSE)

#### Переменные интереса ####

interest_vars <- c("Mean_age_log", "Board_size_log", "Gender_variability",
                   "Independence", "Independent_leader", 
                   "Current_exp_log", "Past_exp_log",
                   "Number_of_committees", "Foreign_directors")

control_vars <- c("Total_assets_log", "Age_of_company_log", 
                  "Year_hot", "Tech_sector", 
                  "ROA", "Net_Debt_EBITDA", "Current_ratio", "EBITDA_Margin")

#### Функция для создания датасетов ####

prepare_regression_data <- function(data, underpricing_var, undp_var, save_path, day_label) {
  
  for (var in interest_vars) {
    
    # Линейная регрессия (Underpricing)
    df_lin <- dplyr::select(data, !!underpricing_var, !!var, all_of(control_vars))
    df_lin <- na.omit(df_lin)
    saveRDS(df_lin, file = paste0(save_path, "/", day_label, "_", var, "_lin.rds"))
    
    # Логистическая регрессия (UNDP)
    df_log <- dplyr::select(data, !!undp_var, !!var, all_of(control_vars))
    df_log <- na.omit(df_log)
    saveRDS(df_log, file = paste0(save_path, "/", day_label, "_", var, "_log.rds"))
  }
}

#### --- ДЕНЬ 1 --- ####

prepare_regression_data(data_day1_log, "Underpricing_1", "UNDP_5", "output/regressions/raw_log/day1", "day1")

#### --- ДЕНЬ 30 --- ####

prepare_regression_data(data_day30_log, "Underpricing_30", "UNDP_5", "output/regressions/raw_log/day30", "day30")
