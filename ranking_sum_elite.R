# For Linux
# Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
# For Windows
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")

library(dplyr)
library(googlesheets)

results_source = "googlesheets"
ranking_type = "elite"
max_amount_of_starts_counted_for_sum = 7

coefs_comps <- data.frame()

if(results_source == "googlesheets") {
  # Возможно, попросит аутентификации в браузере!
  require(googlesheets)
  
  # Читаем список всех доступных файлов для будущего использования
  my_sheets <- gs_ls()
  
  reference_database <- as.data.frame(gs_read(gs_title("Orienteers database"), ws = format(Sys.Date(), "%Y")))
  
  if(ranking_type == "elite") {
    coefs_comps <- as.data.frame(gs_read(gs_title("Elite Ranking Starts"), ws = format(Sys.Date(), "%Y")))
  } else {
    stop("Unsupported rating type!")
  }
} else {
  if(results_source == "local") {
    # Or do it all locally
    reference_database <- read.csv2(file = "orienteers_database.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
    
    if(ranking_type == "elite") {
      print("Выберите файл с описанием рейтинговых стартов и их коэффициентов для ветеранского рейтинга.")
      coefs_comps_filename <- file.choose()
      coefs_comps <- read.csv2(coefs_comps_filename, encoding = "UTF-8", stringsAsFactors = FALSE,
                               colClasses = c(rep("character", 4), "double"))
    } else {
      stop("Unsupported rating type!")
    }
  } else {
    stop("Unsupported results source!")
  }
}

result_list <- list() #create an empty list

passed_comps <- coefs_comps[!is.na(coefs_comps$`Ссылка на результаты`), ]

for (i in 1:nrow(passed_comps)) {
  if(results_source == "googlesheets") {
    # Ищем тот документ, который соответствует дате
    results_filename <- paste0(passed_comps$Дата[i], "_", passed_comps$Название[i], "_", passed_comps$Вид[i])
    results_sheet <- gs_title(results_filename)
    print(results_sheet)
    # Читаем файл результатов
    result_list[[i]] <- as.data.frame(gs_read(ss = results_sheet, ws = paste0("scores_", ranking_type, "_ranking")))
  } else {
    if(results_source == "local") {
      result_list[[i]] <- read.csv2(file = file.path(getwd(), list.files(file.path(getwd()),
                                                                         pattern = paste0("^", coefs_comps$Дата[i],
                                                                                          ".*", ranking_type, "_ranking\\.csv"))),
                                    encoding = "UTF-8", stringsAsFactors = FALSE)
    } else {
      stop("Unsupported results source!")
    }
  }
}
all_comps_results <- do.call("rbind",result_list) #combine all vectors into a matrix

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

# Оставляем в базе только членов БФО
reference_database <- reference_database[! is.na(reference_database$`Член БФО в текущем году`), ]
# Выбираем только нужные колонки из базы
reference_database <- reference_database[, c("ФИ", "ГР", "Группа")]
# Так как в элитном рейтинге могут принимать участие спортсмены из любой группы, оставляем только первую букву группы (= пол)
reference_database$`Группа` <- substr(reference_database$`Группа`, 1, 1)

sum <- left_join(reference_database, results_sum, by = c("ФИ", "ГР"))

# Сортируем
sum <- sum[order(sum$Группа, -sum$Сумма), ]

# Переименовываем колонки с очками так, как больше нравится Диме Давидовичу
colnames(sum)[grep("Очки_\\d{8}", colnames(sum))] <- format(strptime(substring(colnames(sum)[grep("Очки_\\d{8}", colnames(sum))], 10, 14), format = "%m%d"), format = "%d.%m")

# Убираем людей, которые в элитном рейтинге участия не принимали (у них в сумме не 0, а NA)
if (ranking_type == "elite") {
  sum = filter(sum, ! is.na(Сумма))
}

library(xlsx) #load the package
filename = paste0(ranking_type, "_ranking_sum_by_date_", last(passed_comps$Дата), ".xlsx")

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
