
library(dplyr)

reference_database <- read.csv2(file = "youth_and_junior_database.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

results_20170401 <- read.csv2(file = "results/20170401_Брестский_Подснежник_классика_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170402 <- read.csv2(file = "results/20170402_Брестский_Подснежник_спринт_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170426 <- read.csv2(file = "results/20170426_Кубок_Гродно_средняя_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170427 <- read.csv2(file = "results/20170427_Кубок_Гродно_спринт_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170429 <- read.csv2(file = "results/20170429_Майская_Многодневка_средняя_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170430 <- read.csv2(file = "results/20170430_Майская_Многодневка_классическая_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170508 <- read.csv2(file = "results/20170508_Командный_Чемпионат_средняя_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170509 <- read.csv2(file = "results/20170509_Командный_Чемпионат_классическая_очки.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

comp_dates <- c("_20170401", "_20170402", "_20170426", "_20170427", "_20170429", "_20170430", "_20170508", "_20170509")

all_rows_results <- rbind(results_20170401,
                          results_20170402,
                          results_20170426,
                          results_20170427,
                          results_20170429,
                          results_20170430,
                          results_20170508,
                          results_20170509)
whom_to_add <- anti_join(all_rows_results, reference_database, by = c("ФИ", "ГР"))
whom_to_add <- filter(whom_to_add, !duplicated(whom_to_add[, c("ФИ", "ГР")]))
write.csv2(x = select(whom_to_add, ФИ, Коллектив, Квал, ГР, Группа), file = "whom_to_add.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("ВНИМАНИЕ! Проверить, не нужно ли добавить участников в общую базу!")

results_20170401 <- select(results_20170401, ФИ, ГР, Очки_20170401 = Очки)
results_20170402 <- select(results_20170402, ФИ, ГР, Очки_20170402 = Очки)
results_20170426 <- select(results_20170426, ФИ, ГР, Очки_20170426 = Очки)
results_20170427 <- select(results_20170427, ФИ, ГР, Очки_20170427 = Очки)
results_20170429 <- select(results_20170429, ФИ, ГР, Очки_20170429 = Очки)
results_20170430 <- select(results_20170430, ФИ, ГР, Очки_20170430 = Очки)
results_20170508 <- select(results_20170508, ФИ, ГР, Очки_20170508 = Очки)
results_20170509 <- select(results_20170509, ФИ, ГР, Очки_20170509 = Очки)

results <- full_join(x = results_20170401, results_20170402, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170426, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170427, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170429, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170430, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170508, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170509, by = c("ФИ" = "ФИ", "ГР" = "ГР"))

# Добавляем, перечитываем базу, проверяем, всех ли добавили

# Теперь можно считать сумму
results$Сумма <- apply(X = select(results, starts_with("Очки")),
                       MARGIN = 1,
                       FUN = function(x) {sum(sort(x, decreasing = TRUE)[1:5], na.rm = TRUE)})

sum <- left_join(reference_database, results, by = c("ФИ", "ГР"))

write.csv2(x = sum,
           file = paste0("results/sum_by_date", last(comp_dates), ".csv"),
           row.names = FALSE,
           fileEncoding = "UTF-8", na = "")
