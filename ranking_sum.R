
library(dplyr)
library(googlesheets)

reference_database <- as.data.frame(gs_read(gs_title("Youth and Juniors database")))
# reference_database <- read.csv2(file = "youth_and_junior_database.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

results_20170401 <- read.csv2(file = "results/20170401_Брестский подснежник_классика_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170402 <- read.csv2(file = "results/20170402_Брестский подснежник_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170423 <- read.csv2(file = "results/20170423_Открытый Кубок Гродно_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170424 <- read.csv2(file = "results/20170424_Открытый Кубок Гродно_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170508 <- read.csv2(file = "results/20170508_Командный чемпионат РБ_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170509 <- read.csv2(file = "results/20170509_Командный чемпионат РБ_классика_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170612 <- read.csv2(file = "results/20170612_Юношеское первенство РБ_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170613 <- read.csv2(file = "results/20170613_Юношеское первенство РБ_классика_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170617 <- read.csv2(file = "results/20170617_Открытый Кубок Шклова_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170618 <- read.csv2(file = "results/20170618_Открытый Кубок Шклова_классика_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170909 <- read.csv2(file = "results/20170909_Мемориал Машерова_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170910 <- read.csv2(file = "results/20170910_Мемориал Машерова_классика_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170916 <- read.csv2(file = "results/20170916_Чемпионат Минской области_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170917 <- read.csv2(file = "results/20170917_Чемпионат Минской области_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170923 <- read.csv2(file = "results/20170923_Кубок ДЮК_средняя_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170924 <- read.csv2(file = "results/20170924_Кубок ДЮК_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20170930 <- read.csv2(file = "results/20170930_Золотая осень_классика_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
results_20171001 <- read.csv2(file = "results/20171001_Золотая осень_спринт_очки_рейтинг.csv", encoding = "UTF-8", stringsAsFactors = FALSE)


comp_dates <- c("20170401",
                "20170402",
                "20170423",
                "20170424",
                "20170508",
                "20170509",
                "20170612",
                "20170613",
                "20170617",
                "20170618",
                "20170909",
                "20170910",
                "20170916",
                "20170917",
                "20170923",
                "20170924",
                "20170930",
                "20171001")

all_rows_results <- rbind(results_20170401,
                          results_20170402,
                          results_20170423,
                          results_20170424,
                          results_20170508,
                          results_20170509,
                          results_20170612,
                          results_20170613,
                          results_20170617,
                          results_20170618,
                          results_20170618,
                          results_20170909,
                          results_20170910,
                          results_20170916,
                          results_20170917,
                          results_20170923,
                          results_20170924,
                          results_20170930,
                          results_20171001)
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
results_20170909 <- select(results_20170909, ФИ, ГР, Очки_20170909 = Очки)
results_20170910 <- select(results_20170910, ФИ, ГР, Очки_20170910 = Очки)
results_20170916 <- select(results_20170916, ФИ, ГР, Очки_20170916 = Очки)
results_20170917 <- select(results_20170917, ФИ, ГР, Очки_20170917 = Очки)
results_20170923 <- select(results_20170923, ФИ, ГР, Очки_20170923 = Очки)
results_20170924 <- select(results_20170924, ФИ, ГР, Очки_20170924 = Очки)
results_20170930 <- select(results_20170930, ФИ, ГР, Очки_20170930 = Очки)
results_20171001 <- select(results_20171001, ФИ, ГР, Очки_20171001 = Очки)

results <- full_join(x = results_20170401, results_20170402, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170423, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170424, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170508, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170509, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170612, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170613, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170617, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170618, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170909, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170910, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170916, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170917, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170923, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170924, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20170930, by = c("ФИ" = "ФИ", "ГР" = "ГР"))
results <- full_join(x = results, y = results_20171001, by = c("ФИ" = "ФИ", "ГР" = "ГР"))

# Теперь можно считать сумму
results$Сумма <- apply(X = select(results, starts_with("Очки")),
                       MARGIN = 1,
                       FUN = function(x) {sum(sort(x, decreasing = TRUE)[1:ifelse(length(x) < 10, length(x), 10)], na.rm = TRUE)})

results$Среднее <- apply(X = select(results, starts_with("Очки")),
                       MARGIN = 1,
                       FUN = function(x) {round(mean(sort(x, decreasing = TRUE)[1:ifelse(length(x) < 10, length(x), 10)], na.rm = TRUE))})


sum <- left_join(reference_database, results, by = c("ФИ", "ГР"))

# Сортируем
sum <- sum[order(sum$Группа, -sum$Сумма), ]

library(xlsx) #load the package
filename = paste0("results/ranking_sum_by_date_", last(comp_dates), ".xlsx")

for(i in sort(unique(sum$Группа))) {
  if(!file.exists(filename)) {
    x = filter(sum, Группа == i)
    x = cbind(`№` = 1:nrow(x), x, Место = 1:nrow(x))
    write.xlsx(x, file = filename,
               sheetName = i, row.names = FALSE, showNA = FALSE)
  } else {
    x = filter(sum, Группа == i)
    x = cbind(`№` = 1:nrow(x), x, Место = 1:nrow(x))
    write.xlsx(x, file = filename, append = TRUE,
               sheetName = i, row.names = FALSE, showNA = FALSE)
  }
}
