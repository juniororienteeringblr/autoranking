ranking_scores_junior <- function(competition_date = NA,
                           competition_name = NA,
                           competition_distance = NA,
                           ranking_type = c("youth", "junior"),
                           competition_coefficient = NA) {
  # Шапка файлов результатов должна быть следующей
  # ФИ;Коллектив;Квал;Номер;ГР;Результат;Место;Прим;Группа
  
  print(paste("Computing ranking for date:", competition_date, "and coefficient", competition_coefficient))

  require(googlesheets4)
  
  # Ищем тот документ, который соответствует дате
  print(paste("Searching for the file", paste0(competition_date, "_", competition_name, "_", competition_distance)))
  googlesheet_results_name = paste0(competition_date, "_", competition_name, "_", competition_distance)
  results_sheet <- drive_find(pattern = googlesheet_results_name, type = "spreadsheet")
  
  print(results_sheet)
  
  # Читаем файл результатов
  results <- as.data.frame(read_sheet(ss = results_sheet, sheet="results", col_types='ccciicccc'))
  
  # Читаем базу ориентировщиков
  reference_database <- as.data.frame(read_sheet(drive_find(pattern = "Orienteers database",
                                                            type = "spreadsheet", n_max=1),
                                                 sheet = format(Sys.Date(), "%Y"),
                                                 col_types='ccciccccccccc'))
  
  library(stringi)
  library(stringr)
  results$`ФИ` <- str_to_title(results$`ФИ`)
  results$`ФИ` <- str_replace_all(results$`ФИ`, stri_enc_toutf8("ё"), stri_enc_toutf8("е"))
  results$`ФИ` <- str_replace_all(results$`ФИ`, stri_enc_toutf8("Ё"), stri_enc_toutf8("Е"))
  
  # Убираем иностранных спортсменов
  results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`ФИ`), ]
  results <- results[!grepl("(Ukr)|(Rus)|(RUS)|(rus)|(Mda)|(Lat)|(Est)|(EST)|(UKR)|(ukr)|(HUN)|(hun)|(FIN)|(POL)|(pol)$", results$`Коллектив`), ]
  
  library(dplyr)
  # Убираем спортсменов-не членов федерации
  reference_database <- reference_database[! is.na(reference_database$`Член БФО в текущем году`), ]
  # Убираем тех, кто оплатил взнос после даты соревнований
  reference_database <- reference_database[as.Date(reference_database$`Дата оплаты`, '%d.%m.%Y') <= as.Date(competition_date, '%Y%m%d'), ]
  results <- semi_join(results, reference_database, by = c("ФИ", "ГР"))
  
  # У сошедших в графе "место" стоит 0, у участников в/к - "в/к"
  results$`Место`[results$`Место` == "в/к"] <- "0"
  results$`Место` <- as.numeric(results$`Место`)
  results$`Результат_сек` <- as.numeric(as.difftime(as.character(results$`Результат`), format = "%H:%M:%S", units = "secs"))
  
  # coefs_group <- read.csv2(file = "group_coef.txt", encoding = "UTF-8", stringsAsFactors = FALSE,
  #                          colClasses = c("character", "double"))
  
  # results$`Сложность` <- substr(results$`Группа`, str_locate(results$`Группа`, paste0("(", paste0(coefs_group$`Сложность`, collapse = "|"), ")")), 1000)
  results$`Сложность` <- substr(results$`Группа`, 2, 1000)
  # Выбираем только группу 20 и сильнейшую из элитных групп
  # ЕЩЕ НЕ СДЕЛАНО, СДЕЛАТЬ!!!
  results <- results %>% filter(`Сложность` == stri_enc_toutf8("20") |
                                  `Сложность` == stri_enc_toutf8("21") |
                                  `Сложность` == stri_enc_toutf8("21E") |
                                  `Сложность` == stri_enc_toutf8("21Е"))
  
  library(dplyr)
  # results <- left_join(results, coefs_group, by = c("Сложность" = "Сложность"))
  
  # И чтобы не было в/к шников удаляем вообще их результаты
  results <- results %>% filter(`Место` != "0")
  
  winning_time <- results %>% filter(`Место` != 0) %>% group_by(`Группа`) %>% filter(!is.na(`Результат_сек`)) %>% summarise(`Время_победителя` = min(`Результат_сек`))

  results <- left_join(results, winning_time, by = c("Группа" = "Группа"))
  
  # results <- mutate(results, `Очки` = round(competition_coefficient*1000*`Коэффициент`*(2* (`Время_победителя` / `Результат_сек`) - 1)))
  results <- mutate(results, `Очки` = round(competition_coefficient*(2* (`Время_победителя` / `Результат_сек`) - 1)))
  # results$`Очки`[results$`Очки` <= 1] <- 1
  results$`Очки`[results$`Очки` < 1] <- 0
  results$`Очки`[is.na(results$`Очки`)] <- 0
  results$`Очки`[results$`Место` == 0] <- 0
  
  names(results) <- stri_enc_toutf8(names(results), is_unknown_8bit = FALSE, validate = FALSE)
  
  results <- as.data.frame(results)
  
  # Выбираем только людей подходящих групп по возрасту
  if(ranking_type == "youth") {
    results <- results[as.integer(format(Sys.Date(), "%Y")) - results$`ГР` <= 18, ]
  } else {
    if(ranking_type == "junior") {
      results <- results[between(as.integer(format(Sys.Date(), "%Y")) - results$`ГР`, 19, 20), ]
    } else {
      stop("Unsupported ranking type!")
    }
  }
  
  sheet_name = paste0("scores_", ranking_type, "_ranking")
  
  if(sheet_name %in% sheet_names(results_sheet)) {
    sheet_delete(results_sheet, sheet_name)
  }
  sheet_write(results, ss = results_sheet, sheet = sheet_name)
  
  rm(coefs_group, results, winning_time)
}
