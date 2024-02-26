library(hcandersenr)
library(textdata)
library(syuzhet)
library(tidytext)
library(udpipe)
library(dplyr)


# Получить тексты сказок Ханса Кристиана Андерсена
tales <- hcandersenr::hcandersen_en

#лематизируем все сказки
udpipe_download_model(language = "english-gum")
eng_gum <- udpipe_load_model(file = "english-gum-ud-2.5-191206.udpipe")

tales_ann <- udpipe_annotate(eng_gum, x = tales$text, tales$book)

#сделаем тиббл и удалим ненужные столбцы и pos
tbl_tales_lema <- as_tibble(tales_ann) %>% 
  filter(upos != "PUNCT") %>% 
  rename(tale = doc_id) %>% 
  select(-paragraph_id, -sentence, -xpos)

#Разделим все на отрывки по 100 слов
tbl_tales <- tbl_tales_lema %>% 
  select(tale, lemma) %>% 
  rename(token = lemma) %>% 
  mutate(chunk = round(((row_number() + 50) / 100), 0))
#Получили 4171 отрывка


# Оценить сентимент каждой сказки с помощью ранжированного тезауруса "afinn"
sentiments <- get_sentiments("afinn") 

#переименуем столбец 
sentiments <- sentiments %>% 
  rename(token = word ) 

#Соединение лексикона и текста
tales_sent <- tbl_tales %>%
  inner_join(sentiments, by = "token")

#изменим некоректные значения(например war, которое имело значение всего -2 и dead)
tales_sent <- tales_sent %>%
  mutate(value = ifelse(token == "war", -5, value)) %>% 
  mutate(value = ifelse(token == "dead", -5, value)) 

#поменям класс колонки "value" на числовой
tales_sent$value <- as.numeric(tales_sent$value)

# Фильтруем строки, где значение от -1 до -5
result1 <- tales_sent %>%
  group_by(tale) %>%
  filter(value == min(value))  

#Проссумируем все отрицательные числа и групперуем по сказке
result1 <- result1 %>%
  group_by(tale) %>%
  summarise(total_value = sum(value))

result1 <- result1 %>% 
  arrange((total_value))

#в итоге самой грустной/трагической сказкой является "The marsh king's daughter"
#суммарно она набрала -140 баллов - наинизший результат

#"Дочь болотного короля" действительно является одной из самых трагичных и печальных сказок,
#написанных Гансом Кристианом Андерсеном. 
#В ней рассказывается история молодой девушки, которая сталкивается с многочисленными испытаниями
#и невзгодами, включая ее неразделенную любовь к принцу и конечную жертву, которую она приносит ради него.
#Сказка наполнена темами тоски, несбывшейся любви и борьбы главного героя.
#Хотя трудно однозначно назвать какую-либо конкретную сказку "самой печальной", "Дочь болотного короля", безусловно,
#входит в число самых пронзительных и печальных сказок Андерсена, по нашему исследованию
