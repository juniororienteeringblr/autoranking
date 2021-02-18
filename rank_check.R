library("xlsx")

working_dir <- 'C:/Users/Ann/Google Диск/БФО/3 - Соревнования и мероприятия/Результаты соревнований'
sportsmen_reference_filename <- '20210218_БФО_спортсмены.xlsx'
competition_participants_filename <- '20210218_ЧРБ лыжный_список участников.xlsx'

competition_database <- read.xlsx(paste(working_dir, competition_participants_filename,
                                        sep='/'), sheetIndex=1, header=TRUE,
                                  encoding = "UTF-8")
reference_database <- read.xlsx(paste(working_dir, sportsmen_reference_filename,
                                      sep='/'), sheetIndex=1, header=TRUE,
                                encoding = "UTF-8")

library(stringi)
library(stringr)
competition_database$`ФИО` <- str_to_title(competition_database$`ФИ`)
competition_database$`ФИО` <- str_replace_all(competition_database$`ФИ`,
                                              stri_enc_toutf8("ё"),
                                              stri_enc_toutf8("е"))
competition_database$`ФИО` <- str_replace_all(competition_database$`ФИ`,
                                              stri_enc_toutf8("Ё"),
                                              stri_enc_toutf8("Е"))

reference_database$`Название` <- str_to_title(reference_database$`Название`)
reference_database$`Название` <- str_replace_all(reference_database$`Название`,
                                                 stri_enc_toutf8("ё"),
                                                 stri_enc_toutf8("е"))
reference_database$`Название` <- str_replace_all(reference_database$`Название`,
                                                 stri_enc_toutf8("Ё"),
                                                 stri_enc_toutf8("Е"))

library(dplyr)

# Incorrect ranks
should_be_rank <- left_join(competition_database, reference_database,
                        by = c('ФИО'='Название', 'Г.р.'='Год.рождения'))
#colnames(should_be_rank)
should_be_rank <- should_be_rank[c('ФИО', 'Г.р.', 'Разряд.x', 'Клуб.x',
                                   'Активность', 'Разряд.y', 'Клуб.y')]
for (i in 1:nrow(should_be_rank)) {
  rank_entered <- should_be_rank[i, 'Разряд.x']
  rank_reference <- should_be_rank[i, 'Разряд.y']
  
  if(!is.na(rank_reference) & rank_entered != rank_reference) {
    print(paste(should_be_rank[i, 'ФИО'], 'заявлен(а) как', rank_entered,
                '- разряд в базе', rank_reference))
  }
}

# Incorrect years of birth
should_be_year <- left_join(competition_database, reference_database,
                            by = c('ФИО'='Название'))
#colnames(should_be_year)
should_be_year <- should_be_year[c('ФИО', 'Г.р.', 'Год.рождения')]
for (i in 1:nrow(should_be_year)) {
  year_entered <- should_be_year[i, 'Г.р.']
  year_reference <- should_be_year[i, 'Год.рождения']
  
  if(!is.na(year_reference) & year_entered != year_reference) {
    print(paste(should_be_year[i, 'ФИО'], 'заявлен с годом рождения ', year_entered,
                '- год рождения в базе', year_reference))
  }
}

# Maybe incorrect names
not_found_in_reference_database <- anti_join(competition_database,
                                             reference_database,
                                             by = c('ФИО'='Название'))
reference_database$Фамилия <- sapply(reference_database$Название, function(x){str_split(x, " ")[[1]][1]})
library(stringdist)
for (i in 1:nrow(not_found_in_reference_database)) {
  name_entered <- not_found_in_reference_database[i, 'ФИО']
  name_splitted <- str_split(name_entered, " ")[[1]]
  surname <- name_splitted[1]
  if (length(name_splitted) != 2) {
    
  } else {
    name_surname_opposite <- paste(name_splitted[2], name_splitted[1])
  }
  
  candidate <- reference_database$Название[amatch(name_entered, reference_database$Название, maxDist=3)]
  candidate_2 <- reference_database$Название[amatch(name_surname_opposite, reference_database$Название, maxDist=3)]
  same_surname <- reference_database$Название[amatch(surname, reference_database$Фамилия, maxDist=1)]
  
  # Typos
  if (!is.na(candidate)) {
    print(paste(name_entered, 'looks like reference', candidate))
  }
  # Mixed name and surname
  if (!is.na(candidate_2)) {
    print(paste(name_entered, 'looks like reversed of', candidate_2))
  }
  # Wrong name
  if (!is.na(same_surname)) {
    print(paste(name_entered, 'same surnames in database for', same_surname))
  }
}

# Whom to add
print("Add to the database")
not_found_in_reference_database[c('ФИО', "Г.р.", "Разряд", "Клуб", "Чип")]
