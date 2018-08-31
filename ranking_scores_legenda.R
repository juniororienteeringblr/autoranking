Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

date_string <- "20180601"

results_filename <- paste0("C:/Studies/autoranking/for_legenda/", date_string, "r.csv")

results <- read.csv(results_filename, encoding = "1251",
                     colClasses = c("character", "character", "character", "integer",
                                    "integer", "character", "character", "character",
                                    "character"))

library(stringi)
library(stringr)
results$`ФИ` <- str_to_title(results$`ФИ`)
results$`ФИ` <- str_replace_all(results$`ФИ`, c("ё" = "е", "Ё" = "Е"))

# У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
results$`Место`[results$`Место` == "в/к"] <- "0"
results$`Место` <- as.numeric(results$`Место`)
results$`Результат_сек` <- as.numeric(as.difftime(as.character(results$`Результат`), format = "%H:%M:%S", units = "secs"))

coefs_group <- read.csv2(file = "C:/Studies/autoranking/for_legenda/group_coef_legenda.txt", encoding = "UTF-8", stringsAsFactors = FALSE,
                         colClasses = c("character", "double"))

results$`Сложность` <- substr(results$`Группа`, str_locate(results$`Группа`, paste0("(", paste0(coefs_group$`Сложность`, collapse = "|"), ")")), 1000)
library(dplyr)
results <- left_join(results, coefs_group, by = c("Сложность" = "Сложность"))

results$`Скорость` <- (as.numeric(results$`Длина`)/1000)/(results$`Результат_сек`/3600)

results <- mutate(results, `Очки` = round(`Коэффициент`*`Скорость`/6.5))

# Вычисляем правильную возрастную группу для человека

results$`Возраст` <- 2018 - results$`ГР`

results$`Возрастная_группа` <- paste0(substr(results$`Группа`, 1, 1),
                                 ifelse(results$`Возраст` > 20, "21",
                                        ifelse(results$`Возраст` > 18, "20",
                                               ifelse(results$`Возраст` > 16, "18",
                                                      ifelse(results$`Возраст` > 14, "16",
                                                             ifelse(results$`Возраст` > 12, "14",
                                                                    ifelse(results$`Возраст` > 10, "12",
                                                                           ifelse(results$`Возраст` > 8, "10", "8"))))))))

results_towrite <- results[, c("ФИ", "Возрастная_группа", "ГР", "Результат", "Место", "Группа", "Очки", "Коллектив")]

# Сортируем
results_towrite <- results_towrite[order(results_towrite$Возрастная_группа, -results_towrite$Очки), ]

write.csv(results_towrite,
          file = paste0("C:/Studies/autoranking/for_legenda/", date_string, "r_для_награждения.csv"),
          row.names = FALSE)

# Поревьюить и поправить, если нужно, результаты для награждения!!!

results_for_team <- read.csv(file = paste0("C:/Studies/autoranking/for_legenda/", date_string, "r_для_награждения.csv"))

results_for_team <- filter(results_for_team, ! (`Возрастная_группа` %in% c("Ж21", "М21")))

sum_by_team <- results_for_team %>%
  group_by(`Коллектив`) %>% top_n(10, `Очки`) %>%
  group_by(`Коллектив`) %>% summarise(`Сумма_очков_коллектив` = sum(`Очки`))

# Сортируем
sum_by_team <- sum_by_team[order(-sum_by_team$`Сумма_очков_коллектив`), ]

write.csv(sum_by_team, file = paste0("C:/Studies/autoranking/for_legenda/", date_string, "r_кубок_коллективы.csv"), row.names = FALSE)

# Наконец, когда будут два файла, считаем сумму для коллективов по двум этапам
sum_team_1 <- read.csv(file = "C:/Studies/autoranking/for_legenda/20180601r_кубок_коллективы.csv")
names(sum_team_1) <- c("Коллектив", "Сумма_очков_1")
sum_team_2 <- read.csv(file = "C:/Studies/autoranking/for_legenda/20180831r_кубок_коллективы.csv")
names(sum_team_2) <- c("Коллектив", "Сумма_очков_2")

sum_teams <- full_join(sum_team_1, sum_team_2, by = c("Коллектив"))

sum_teams[is.na(sum_teams)] <- 0

sum_teams$`Сумма_Кубок` <- sum_teams$`Сумма_очков_1` + sum_teams$`Сумма_очков_2`

# Сортируем
sum_teams <- sum_teams[order(-sum_teams$`Сумма_Кубок`), ]

write.csv(sum_teams, file = "C:/Studies/autoranking/for_legenda/Кубок_2018_результат.csv", row.names = FALSE)
