# For Linux
# Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
# For Windows
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

library(dplyr)
require(googledrive)
require(googlesheets4)

results_source = "googlesheets"
ranking_type = 'youth'  # "youth" 'junior' 'youth_selection'
max_amount_of_starts_counted_for_sum = 7

coefs_comps <- data.frame()

# Возможно, попросит аутентификации в браузере!
reference_database <- as.data.frame(read_sheet(drive_find(pattern = "Orienteers database",
                                                          type = "spreadsheet", n_max=1),
                                               sheet = format(Sys.Date(), "%Y"),
                                               col_types='ccciccccccccc'))

if(ranking_type == "youth") {
  googlesheet_name <- "Youth Ranking Starts"
} else {
  if(ranking_type == "junior") {
    googlesheet_name <- "Junior Ranking Starts"
  } else {
    if(ranking_type == "youth_selection") {
      googlesheet_name <- "Youth Selection Starts"
    } else {
      stop("Unsupported rating type!")
    }
  }
}

coefs_comps <- as.data.frame(read_sheet(drive_find(pattern = googlesheet_name,
                                                   type = "spreadsheet"),
                                        sheet = format(Sys.Date(), "%Y")))

result_list <- list() #create an empty list

passed_comps <- coefs_comps[!is.na(coefs_comps$`Ссылка на результаты`), ]

for (i in 1:nrow(passed_comps)) {
  # Ищем тот документ, который соответствует дате
  results_filename <- paste0(passed_comps$Дата[i], "_", passed_comps$Название[i], "_", passed_comps$Вид[i])
  results_sheet <- drive_find(pattern = results_filename, type = "spreadsheet")
  
  # Читаем файл результатов
  if(ranking_type == "youth") {
    sheet_name = paste0("scores_", ranking_type, "_ranking")
  } else {
    if(ranking_type == "junior") {
      sheet_name = paste0("scores_", ranking_type, "_ranking")
    } else {
      if(ranking_type == "youth_selection") {
        sheet_name = paste0("scores_", ranking_type)
      } else {
        stop("Unsupported rating type!")
      }
    }
  }
  
  result_list[[i]] <- as.data.frame(read_sheet(ss = results_sheet, sheet=sheet_name, col_types='ccciiciccicdii'))
  
  # readline(prompt="Press [enter] to continue")  # Использовать, если происходит переполнение на стороне Гугла
}
all_comps_results <- do.call("rbind",result_list) #combine all vectors into a matrix

whom_to_add <- anti_join(all_comps_results, reference_database, by = c("ФИ", "ГР"))
whom_to_add <- filter(whom_to_add, !duplicated(whom_to_add[, c("ФИ", "ГР")]))
write.csv2(x = whom_to_add[ , c("ФИ", "Коллектив", "Квал", "ГР", "Группа")], file = "whom_to_add.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("ВНИМАНИЕ! Проверить, не нужно ли добавить участников в общую базу!")

# Добавляем, перечитываем базу, проверяем, всех ли добавили

for(i in 1:nrow(passed_comps)) {
  result_list[[i]] <- result_list[[i]][, c("ФИ", "ГР", "Очки")]
  names(result_list[[i]])[names(result_list[[i]]) == 'Очки'] <- paste0("Очки_", passed_comps$Дата[i])
}

results_sum <- data.frame(ФИ = character(), ГР = integer())

for(i in 1:nrow(passed_comps)) {
  results_sum <- full_join(x = results_sum, result_list[[i]], by = c("ФИ" = "ФИ", "ГР" = "ГР"))
}

# Теперь можно считать сумму
results_sum$Сумма <- apply(X = select(results_sum, starts_with("Очки")),
                           MARGIN = 1,
                           FUN = function(x) {sum(sort(x, decreasing = TRUE)[1:ifelse(length(x) < max_amount_of_starts_counted_for_sum, length(x), max_amount_of_starts_counted_for_sum)], na.rm = TRUE)})

results_sum$Среднее <- apply(X = select(results_sum, starts_with("Очки")),
                             MARGIN = 1,
                             FUN = function(x) {round(mean(sort(x, decreasing = TRUE)[1:ifelse(length(x) < max_amount_of_starts_counted_for_sum, length(x), max_amount_of_starts_counted_for_sum)], na.rm = TRUE))})

# Выбираем только нужные колонки из базы
reference_database <- reference_database[, c("ФИ", "ГР", "Группа")]

sum <- left_join(reference_database, results_sum, by = c("ФИ", "ГР"))

# Так как теперь в рейтинг входят все, в том числе и младшие. переделываем 10 и 8 группы на 12
sum$`Группа`[sum$`Группа` == "М10"] <- "М12"
sum$`Группа`[sum$`Группа` == "Ж10"] <- "Ж12"
sum$`Группа`[sum$`Группа` == "М8"] <- "М12"
sum$`Группа`[sum$`Группа` == "Ж8"] <- "Ж12"

if (ranking_type == "youth_selection") {  # Отбор идет только начиная с групп 14
  sum$`Группа`[sum$`Группа` == "М12"] <- "М14"
  sum$`Группа`[sum$`Группа` == "Ж12"] <- "Ж14"
}

# Сортируем
sum <- sum[order(sum$Группа, -sum$Сумма), ]

# Переименовываем колонки с очками так, как больше нравится Диме Давидовичу
colnames(sum)[grep("Очки_\\d{8}", colnames(sum))] <- format(strptime(substring(colnames(sum)[grep("Очки_\\d{8}", colnames(sum))], 10, 14), format = "%m%d"), format = "%d.%m")

# Убираем неподходящие группы
if(ranking_type == "youth") {
  sum = filter(sum, Группа %in% c("Ж12", "М12", "Ж14", "М14", "Ж16", "М16", "Ж18", "М18"))
} else {
  if(ranking_type == "junior") {
    sum = filter(sum, Группа %in% c("Ж20", "М20"))
  } else {
    if(ranking_type == "youth_selection") {
      sum = filter(sum, Группа %in% c("Ж14", "М14", "Ж16", "М16", "Ж18", "М18"))
    } else {
      stop("Unsupported rating type!")
    }
  }
}

library(xlsx) #load the package

if(ranking_type == "youth") {
  filename = paste0(ranking_type, "_ranking_sum_by_date_", last(passed_comps$Дата), ".xlsx")
} else {
  if(ranking_type == "junior") {
    filename = paste0(ranking_type, "_ranking_sum_by_date_", last(passed_comps$Дата), ".xlsx")
  } else {
    if(ranking_type == "youth_selection") {
      filename = paste0(ranking_type, "_sum_by_date_", last(passed_comps$Дата), ".xlsx")
    } else {
      stop("Unsupported rating type!")
    }
  }
}

for(i in sort(unique(sum$Группа))) {
  if(!file.exists(filename)) {
    x = filter(sum, Группа == i)
    x = cbind(`№` = 1:nrow(x), x, Место = 1:nrow(x))
    x$`Коллектив` <- NULL
    x$`Квал` <- NULL
    x$`Группа` <- NULL
    x <- x[! is.na(x$`Сумма`), ]
    write.xlsx(x, file = filename,
               sheetName = i, row.names = FALSE, showNA = FALSE)
  } else {
    x = filter(sum, Группа == i)
    x = cbind(`№` = 1:nrow(x), x, Место = 1:nrow(x))
    x$`Коллектив` <- NULL
    x$`Квал` <- NULL
    x$`Группа` <- NULL
    x <- x[! is.na(x$`Сумма`), ]
    write.xlsx(x, file = filename, append = TRUE,
               sheetName = i, row.names = FALSE, showNA = FALSE)
  }
}
