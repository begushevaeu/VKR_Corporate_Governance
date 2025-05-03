#### Загрузка пакетов ####

library(dplyr)
library(stargazer)

#### Загрузка данных ####

data_day1_log <- readRDS("data/day1_log.rds")
data_day30_log <- readRDS("data/day30_log.rds")

#### Создание папки для сохранения ####

if (!dir.exists("output/t_tests")) {
  dir.create("output/t_tests")
}

#### Переменные для t-теста ####

numeric_cols <- c("Mean_age_log", "Board_size_log", "Gender_variability",
                  "Independence", "Independent_leader",
                  "Current_exp_log", "Past_exp_log", 
                  "Number_of_committees", 
                  "Foreign_directors", 
                  "Total_assets_log", "Age_of_company_log", 
                  "Year_hot", "Tech_sector", 
                  "ROA", "Net_Debt_EBITDA", "Current_ratio", 
                  "EBITDA_Margin")

#### Пороги ####

limits <- c(0.05, 0.10)

#### Функция для t-тестов ####

run_t_tests <- function(data, limits, underpricing_var, day_label) {
  
  for (limit in limits) {
    
    data$UNDP <- ifelse(data[[underpricing_var]] < limit, 0, 1)
    
    t_test_results <- data.frame()
    
    for (col in numeric_cols) {
      
      group0 <- data %>% filter(UNDP == 0) %>% pull(col) %>% na.omit()
      group1 <- data %>% filter(UNDP == 1) %>% pull(col) %>% na.omit()
      
      if (length(group0) > 1 & length(group1) > 1) {
        
        t_test <- t.test(group0, group1, var.equal = FALSE)
        
        t_test_results <- rbind(t_test_results, data.frame(
          Limit = paste0(limit * 100, "%"),
          Variable = col,
          t_statistic = round(t_test$statistic, 3),
          p_value = round(t_test$p.value, 4),
          mean_UNDP_0 = round(mean(group0), 3),
          mean_UNDP_1 = round(mean(group1), 3)
        ))
      }
    }
    
    #### Сохраняем в HTML ####
    
    filename <- paste0("output/t_tests/t_tests_", day_label, "_log_", limit * 100, ".html")
    
    stargazer(t_test_results, type = "html", summary = FALSE, digits = 2, 
              out = filename)
  }
}

#### === Грязные данные: Первый день === ####

run_t_tests(data_day1_log, limits, "Underpricing_1", "day1")

#### === Грязные данные: Тридцатый день === ####

run_t_tests(data_day30_log, limits, "Underpricing_30", "day30")
