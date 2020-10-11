library(xlsx)

sportsmen <- read.xlsx('C:/Users/Ann/Downloads/спортсмены2020.xlsx', sheetIndex = 1, encoding = "UTF-8")
sportsmen$Коллектив <- gsub('(.*)( \\[[0-9]+\\])', '\\1', sportsmen$Коллектив)
sportsmen$Региональная.федерация <- gsub('(.*)( \\[[0-9]+\\])', '\\1', sportsmen$Региональная.федерация)
sportsmen$Дата.оплаты <- substring(sportsmen$Дата.оплаты, 1, 10)
sportsmen$Членство.в.федерации <- sapply(sportsmen$Членство.в.федерации, function(x){if (grepl('2020', x, fixed = TRUE)) {return('Да')} else {return(NA)}})

reference_database <- as.data.frame(read_sheet(drive_find(pattern = "Orienteers database",
                                                          type = "spreadsheet", n_max=1),
                                               sheet = format(Sys.Date(), "%Y"),
                                               col_types='ccciccccclcl'))

updated_reference <- full_join(reference_database, sportsmen, by = c("ФИ", "ГР"))
