#парсинг сайта

library(rvest)
library(tidyverse)
library(dplyr)
library(stringr)

#подвязываем url с Маяковским
url <- "https://ru.wikisource.org/wiki/%D0%92%D0%BB%D0%B0%D0%B4%D0%B8%D0%BC%D0%B8%D1%80_%D0%92%D0%BB%D0%B0%D0%B4%D0%B8%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B8%D1%87_%D0%9C%D0%B0%D1%8F%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9"
html <- read_html(url)

#создаем тиббл с поэмами 
poems <- tibble(html %>% 
  html_elements('ul:nth-child(78) li') %>% 
  html_text2())

#создаем тиббл с ссылками на поэмы 
url_poems <- tibble(html %>% 
  html_elements('ul:nth-child(78) a') %>% 
  html_attr('href') %>% 
  paste0('https://ru.wikisource.org', .))

#объединяем 2 тиббла в один
poems_Mayakovsky <- tibble(poems, url_poems)

#переименовываем столбцы
colnames(poems_Mayakovsky)[1] ="Поэмы"
colnames(poems_Mayakovsky)[2] ="Ссылка_на_произведение"

#удаляем все "(" ")"
poems_Mayakovsky$Поэмы <- gsub("\\(|\\)", "", poems_Mayakovsky$Поэмы)

#здесь мы с помошью регулярки и условных выражений смогли извлечь года по условию
# если год один то "(19\\d{2})" если несколько лет то "(19)..*"). Возможно, можно было
# сделать проще но при "(19)..*") копируются только года через -, а остальное Na
poems_Mayakovsky <- poems_Mayakovsky %>%
  mutate(Год = ifelse(str_detect(Поэмы, "(19\\d{2})"),
                      str_extract(Поэмы, "(19)..*"), Год))

#удаляем ненужные даты в колонке Поэмы
poems_Mayakovsky$Поэмы <- gsub(" 19.*", "", poems_Mayakovsky$Поэмы)

#создаем ф-цию  для добывания текстов поэм 
poem_url <- "https://ru.wikisource.org/wiki/%D0%9E%D0%B1%D0%BB%D0%B0%D0%BA%D0%BE_%D0%B2_%D1%88%D1%82%D0%B0%D0%BD%D0%B0%D1%85_(%D0%9C%D0%B0%D1%8F%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9)"

#однако на выходе мы получем 4 строки с пустыми данными. Стоит создать новую ф-цию
#которая будет решать эту проблему. Но у "Рабочим Курска, добывшим..." полное отсутствие текста
#на этой странице, поэтому в будущем возьмем данные с другой страницы викитеки
get_text <- function(url) {
  text <- read_html(url) %>% 
  html_elements(".poem p") %>% 
  html_text2() %>% 
  str_c(collapse = " ")
  return(text) }

poems_Mayakovsky <- poems_Mayakovsky %>%
  mutate(Текст_поэмы = map_chr(Ссылка_на_произведение, get_text))

#Сначала заполним строки с отличающимися selectorGadget, но имеющие сходства
temp_tbl <- tibble()
temp_tbl <- rbind(temp_tbl, poems_Mayakovsky[poems_Mayakovsky$Текст_поэмы == "", ])

empty_url <- "https://ru.wikisource.org/wiki/%D0%A0%D0%B0%D0%B1%D0%BE%D1%87%D0%B8%D0%BC_%D0%9A%D1%83%D1%80%D1%81%D0%BA%D0%B0,_%D0%B4%D0%BE%D0%B1%D1%8B%D0%B2%D1%88%D0%B8%D0%BC_%D0%BF%D0%B5%D1%80%D0%B2%D1%83%D1%8E_%D1%80%D1%83%D0%B4%D1%83..._(%D0%9C%D0%B0%D1%8F%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9)"

get_text_empty <- function(url) {
  text <- read_html(url) %>% 
    html_elements(".transparent") %>% 
    html_text2() %>% 
    str_c(collapse = " ")
  return(text) }

temp_tbl <- temp_tbl %>%
  mutate(Текст_поэмы = map_chr(Ссылка_на_произведение, get_text_empty))
#Отлично,добавились только 2 текста... 
#В итоге у нас осталось еще 2 текста, которые в корне отличаются селекторами от остальнх
#Проще добавить оставшиеся в ручную

#дабавим текст "Владимир Ильич Ленин"
lenin_url <- "https://ru.wikisource.org/wiki/%D0%92%D0%BB%D0%B0%D0%B4%D0%B8%D0%BC%D0%B8%D1%80_%D0%98%D0%BB%D1%8C%D0%B8%D1%87_%D0%9B%D0%B5%D0%BD%D0%B8%D0%BD_(%D0%9C%D0%B0%D1%8F%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9)"
lenin_html <- read_html(lenin_url) 

lenin_html <- lenin_html%>% 
  html_elements("div:nth-child(6)") %>% 
  html_text2() %>% 
  str_c(collapse = " ")
  
temp_tbl[3, "Текст_поэмы"] <- lenin_html

#дабавим текст "Рабочим Курска, добывшим..."

workers_url <- "https://ru.wikisource.org/wiki/%D0%A0%D0%B0%D0%B1%D0%BE%D1%87%D0%B8%D0%BC_%D0%9A%D1%83%D1%80%D1%81%D0%BA%D0%B0,_%D0%B4%D0%BE%D0%B1%D1%8B%D0%B2%D1%88%D0%B8%D0%BC_%D0%BF%D0%B5%D1%80%D0%B2%D1%83%D1%8E_%D1%80%D1%83%D0%B4%D1%83..._(%D0%9C%D0%B0%D1%8F%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9)"
workers_html <- read_html(workers_url) 

workers_html <- workers_html%>% 
  html_elements(".transparent") %>% 
  html_text2() %>% 
  str_c(collapse = " ")

temp_tbl[2, "Текст_поэмы"] <- workers_html

#Супер! Все нашлось. Теперь заджойним две таблицы

temp <- poems_Mayakovsky
temp <- poems_Mayakovsky %>%
  left_join(temp_tbl,by="Поэмы")

temp$Текст_поэмы.x[temp$Текст_поэмы.x == ""] <- temp$Текст_поэмы.y[temp$Текст_поэмы.x == ""]
temp$Текст_поэмы.y[temp$Текст_поэмы.x == ""] <- "" 
temp$Текст_поэмы.x[is.na(temp$Текст_поэмы.x)] <- temp$Текст_поэмы.y[is.na(temp$Текст_поэмы.x)]

#удалим ненужные колонки и префиксы, которые появлись после JOIN
temp <- temp[,-5:-7]
names(temp) <- sub("\\.x$", "", names(temp))

#рассортируем все колонки для красоты
PoemsOfMayakovsky <- temp %>% 
  select(Поэмы, Год, Текст_поэмы, Ссылка_на_произведение)

#Итог таков: Хорошо, что были взяты только поэмы, а не все произведения Маяковского
#Из за уникального стиля поэта, произведения довольно тяжело поддаются автоматическому
#скраппингу спомощью selectorGadget (где-то автор пишет новое предложение с абзаца,
#где-то каждое слово на новой строке, а где-то слова разбросаны по странице)

