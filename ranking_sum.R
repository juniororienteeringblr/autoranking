
library(dplyr)
library(googlesheets)

reference_database <- as.data.frame(gs_read(gs_title("Youth and Juniors database")))
# reference_database <- read.csv2(file = "youth_and_junior_database.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

results_20170401 <- read.csv2(file = "results/20170401_Брестский_Подснежник_классика_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170402 <- read.csv2(file = "results/20170402_Брестский_Подснежник_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170423 <- read.csv2(file = "results/20170423_Кубок_Гродно_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170424 <- read.csv2(file = "results/20170424_Кубок_Гродно_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170508 <- read.csv2(file = "results/20170508_Командный_Чемпионат_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170509 <- read.csv2(file = "results/20170509_Командный_Чемпионат_классическая_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170612 <- read.csv2(file = "results/20170612_Первенство_РБ_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170613 <- read.csv2(file = "results/20170613_Первенство_РБ_классическая_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170617 <- read.csv2(file = "results/20170617_Кубок_Шклова_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170618 <- read.csv2(file = "results/20170618_Кубок_Шклова_классическая_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

comp_dates <- c("_20170401", "_20170402", "_20170423", "_20170424", "_20170508", "_20170509", "_20170612", "_20170613", "_20170617", "_20170618")

all_rows_results <- rbind(results_20170401,
                          results_20170402,
                          results_20170423,
                          results_20170424,
                          results_20170508,
                          results_20170509,
                          results_20170612,
                          results_20170613,
                          results_20170617,
                          results_20170618)
whom_to_add <- anti_join(all_rows_results, reference_database, by = c("ФИ", "ГР"))
whom_to_add <- filter(whom_to_add, !duplicated(whom_to_add[, c("ФИ", "ГР")]))
write.csv2(x = select(whom_to_add, ФИ, Коллектив, Квал, ГР, Группа), file = "whom_to_add.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("ВНИМАНИЕ! Проверить, не нужно ли добавить участников в общую базу!")

# Добавляем, перечитываем базу, проверяем, всех ли добавили

results_20170401 <- select(results_20170401, ФИ, ГР, Очки_20170401 = Очки)
results_20170402 <- select(results_20170402, ФИ, ГР, Очки_20170402 = Очки)
results_20170423 <- select(results_20170423, ФИ, ГР, Очки_20170423 = Очки)
results_20170424 <- select(results_20170424, ФИ, ГР, Очки_20170424 = Очки)
results_20170508 <- select(results_20170508, ФИ, ГР, Очки_20170508 = Очки)
results_20170509 <- select(results_20170509, ФИ, ГР, Очки_20170509 = Очки)
results_20170612 <- select(results_20170612, ФИ, ГР, Очки_20170612 = Очки)
results_20170613 <- select(results_20170613, ФИ, ГР, Очки_20170613 = Очки)
results_20170617 <- select(results_20170617, ФИ, ГР, Очки_20170617 = Очки)
results_20170618 <- select(results_20170618, ФИ, ГР, Очки_20170618 = Очки)

results <- full_join(x = results_20170401, results_20170402, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170423, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170424, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170508, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170509, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170612, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170613, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170617, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170618, by = c("ФИ" = "ФИ", "ГР" = "ГР"))


# Теперь можно считать сумму
results$Сумма <- apply(X = select(results, starts_with("Очки")),
                       MARGIN = 1,
                       FUN = function(x) {sum(sort(x, decreasing = TRUE)[1:ifelse(length(x) < 10, length(x), 10)], na.rm = TRUE)})

sum <- left_join(reference_database, results, by = c("ФИ", "ГР"))

write.csv2(x = sum,
           file = paste0("results/ranking_sum_by_date", last(comp_dates), ".csv"),
           row.names = FALSE,
           fileEncoding = "UTF-8", na = "")
