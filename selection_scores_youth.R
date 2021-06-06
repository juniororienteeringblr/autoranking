selection_scores_youth <- function(competition_date = NA,
                           competition_name = NA,
                           competition_distance = NA,
                           ranking_type = c("youth", "junior"),
                           competition_coefficient = NA) {
  # Шапка файлов результатов должна быть следующей
  # ФИ;Коллектив;Квал;Номер;ГР;Результат;Место;Прим;Группа
  
  print(paste("Computing selection for date:", competition_date, "and coefficient", competition_coefficient))

  require(googlesheets4)
  
  # Ищем тот документ, который соответствует дате
  print(paste("Searching for the file", paste0(competition_date, "_", competition_name, "_", competition_distance)))
  googlesheet_results_name = paste0(competition_date, "_", competition_name, "_", competition_distance)
  results_sheet <- drive_find(pattern = googlesheet_results_name, type = "spreadsheet")
  
  print(results_sheet)
  
  # Читаем файл результатов
  results <- as.data.frame(read_sheet(ss = results_sheet, sheet="results", col_types='ccciicccc'))
  
  library(stringi)
  library(stringr)
  results$`ФИ` <- str_to_title(results$`ФИ`)
  results$`ФИ` <- str_replace_all(results$`ФИ`, stri_enc_toutf8("ё"), stri_enc_toutf8("е"))
  results$`ФИ` <- str_replace_all(results$`ФИ`, stri_enc_toutf8("Ё"), stri_enc_toutf8("Е"))
  
  # Убираем иностранных спортсменов
  results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`ФИ`), ]
  results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`Коллектив`), ]
  
  # У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
  results$`Место`[results$`Место` == "в/к"] <- "0"
  results$`Место` <- as.numeric(results$`Место`)
  results$`Результат_сек` <- as.numeric(as.difftime(as.character(results$`Результат`), format = "%H:%M:%S", units = "secs"))
  
  coefs_group <- read.csv2(file = "group_coef.txt", encoding = "UTF-8", stringsAsFactors = FALSE,
                           colClasses = c("character", "double"))
  
  results$`Сложность` <- substr(results$`Группа`, str_locate(results$`Группа`, paste0("(", paste0(coefs_group$`Сложность`, collapse = "|"), ")")), 1000)
  library(dplyr)
  results <- left_join(results, coefs_group, by = c("Сложность" = "Сложность"))
  
  winning_time <- results %>% filter(`Место` != 0) %>% group_by(`Группа`) %>% filter(!is.na(`Результат_сек`)) %>% summarise(`Время_победителя` = min(`Результат_сек`))

  results <- left_join(results, winning_time, by = c("Группа" = "Группа"))
  
  results <- mutate(results, `Очки` = round(1000*`Коэффициент`*(2* (`Время_победителя` / `Результат_сек`) - 1)))
  # results$`Очки`[results$`Очки` <= 1] <- 1
  results$`Очки`[results$`Очки` < 1] <- 0
  results$`Очки`[is.na(results$`Очки`)] <- 0
  results$`Очки`[results$`Место` == 0] <- 0
  
  names(results) <- stri_enc_toutf8(names(results), is_unknown_8bit = FALSE, validate = FALSE)
  
  results <- as.data.frame(results)
  
  # Выбираем только людей подходящих групп по возрасту
  if(ranking_type == "youth_selection") {
    results <- results[as.integer(format(Sys.Date(), "%Y")) - results$`ГР` <= 18, ]
  } else {
    if(ranking_type == "junior_selection") {
      results <- results[between(as.integer(format(Sys.Date(), "%Y")) - results$`ГР`, 19, 20), ]
    } else {
      stop("Unsupported ranking type!")
    }
  }
  
  sheet_name = paste0("scores_", ranking_type)
  
  if(sheet_name %in% sheet_names(results_sheet)) {
    sheet_delete(results_sheet, sheet_name)
  }
  sheet_write(results, ss = results_sheet, sheet = sheet_name)
  
  rm(coefs_group, results, winning_time)
}
