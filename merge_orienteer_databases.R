library(xlsx)
library(stringi)
library(stringr)
library(dplyr)

sportsmen <- read.xlsx('C:/Users/Ann/Google Диск/БФО/3 - Соревнования и мероприятия/Результаты соревнований/20210613_БФО_спортсмены для обновления базы.xlsx', sheetIndex = 1, encoding = "UTF-8")
sportsmen$Клуб <- gsub('(.*)( \\[[0-9]+\\])', '\\1', sportsmen$Клуб)
sportsmen$Региональная.федерация <- gsub('(.*)( \\[[0-9]+\\])', '\\1', sportsmen$Региональная.федерация)
sportsmen$Дата.оплаты <- substring(sportsmen$Дата.оплаты, 1, 10)
sportsmen$Членство.в.федерации <- sapply(sportsmen$Членство.в.федерации, function(x){if (grepl('2020', x, fixed = TRUE)) {return('Да')} else {return(NA)}})
sportsmen$Название <- str_to_title(sportsmen$Название)
sportsmen$Название <- str_replace_all(sportsmen$Название, stri_enc_toutf8("ё"), stri_enc_toutf8("е"))
sportsmen$Название <- str_replace_all(sportsmen$Название, stri_enc_toutf8("Ё"), stri_enc_toutf8("Е"))

reference_database <- as.data.frame(read_sheet(drive_find(pattern = "Orienteers database",
                                                          type = "spreadsheet", n_max=1),
                                               sheet = format(Sys.Date(), "%Y"),
                                               col_types='ccciccccccccci'))

updated_reference <- full_join(reference_database, sportsmen, by = c("ФИ"="Название", "ГР"="Год.рождения", 'Пол'='Пол'))

write.xlsx(updated_reference,
           file = 'C:/Users/Ann/Google Диск/БФО/3 - Соревнования и мероприятия/Результаты соревнований/20210613_БФО_спортсмены merged.xlsx',
           sheetName = 'sportsmen', row.names = FALSE, showNA = FALSE)