library(tidytext)
library(tokenizers)
library(dplyr)
library(stringr)
library(udpipe)
library(ggplot2)
library(rvest)
library(wordcloud2) 

#текст с сайта "Project Gutenberg", который был взят за основу является романом американского писателя Теодора Драйзера "Финансисит"

#подвязываем url с Драйзером и парсим сайт с ссылкой
url <- read_html("https://www.gutenberg.org/ebooks/1840")
#находим ссылку "Plain Text UTF-8"
link <- url %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("1840.txt") %>%
  paste("https://www.gutenberg.org", ., sep="")
#считываем текст
text <- readLines(link)

#создаем таблицу theodore с текстом романа Драйзера и удаляем первые 40 строчек и последние 397(там содержится мусор)
theodore <- tibble(Text_novel = text)
theodore <- theodore %>% 
  slice(41:n()) %>% 
  slice(1:(nrow(theodore)-398))


#создадим нетокинезированную таблицу для будущей лематизации
theo_for_lemma <- theodore %>% 
  mutate(chapter = cumsum(str_detect(Text_novel, "^Chapter [\\DIVXLC]")))


#модифицируем таблицу theodore и добавляем второй столбец с chapter(регулярка ищет нам chapter)
theodore <- theodore %>% 
  mutate(Text_novel = str_to_lower(Text_novel),
         chapter = cumsum(str_detect(Text_novel, "^chapter [\\divxlc]")))

#создаем еще таблицу в которой строки уже разбиты на токены по одному слову
token_theodore <- theodore %>% 
  unnest_tokens(word, Text_novel)

#теперь давайте найдем словесные 2-грамм встречаются чаще всего на протяжении всего романа
grams2_theodore <- theodore %>% 
  unnest_tokens(ngrams, Text_novel, token = 'ngrams', n = 2) %>% 
  filter(!is.na(ngrams)) %>% 
  count(ngrams) %>% 
  arrange(-n)
# "of the" встречается 1095
#предлагаю создать диаграмму топ 10-ти самых частых 2-грамм из всего произведения
top_n <- grams2_theodore %>% 
  top_n(10, n)

ggplot(data = top_n, aes(x = reorder(ngrams, n), y = n, fill = ngrams)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(x = '2-grams', y = 'amount') +
  ggtitle("top10 2-grams in The Financier")

#Подсчетаем кол-во упомянаний персонажей в романе Драйзера "Финансист"

#Лематизация 1 главы
#Используем ранее созданную  нетокинезированную таблицу 'theo_for_lemma'
lema_theodore <- theo_for_lemma %>% 
  filter(chapter == 1)

udpipe_download_model(language = "english-gum")

eng_gum <- udpipe_load_model(file = "english-gum-ud-2.5-191206.udpipe")
financier_ann <- udpipe_annotate(eng_gum, x = lema_theodore$Text_novel)

tbl_financier_lema <- as_tibble(financier_ann) 

#Предлагаю получить интересные данные из таблицы. Найдем топ-10 самых
#часто встречающиеся именсобственных(предположительно это'Frank', 'Cowperwood', 'Butler')
all_lema_theodore <- theo_for_lemma 
eng_gum <- udpipe_load_model(file = "english-gum-ud-2.5-191206.udpipe")
all_chapters_financier_ann_ <- udpipe_annotate(eng_gum, x = all_lema_theodore$Text_novel)

tbl_all_chapters_lemma <- as_tibble(all_chapters_financier_ann_) 

PROPN <- tbl_all_chapters_lemma %>%
  filter(upos == 'PROPN') %>% 
  count(lemma) %>% 
  arrange(-n) %>% 
  top_n(13) %>% 
#удалилим ненужные артефакты
  filter(lemma != 'Mr' & lemma != 'Mr.' & lemma !=  'Street')

#Отобразим на диаграме
ggplot(data = PROPN, aes(x = reorder(lemma, n), y = n, fill = lemma)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  guides(fill = FALSE) +
  labs(x = 'сharacter', y = 'frequency') 

markup <- tbl_all_chapters_lemma %>% 
  group_by(upos) %>% 
  count() %>% 
  filter(upos != "PUNCT") %>% 
  arrange(-n) %>% 
  top_n(14, upos)

#облако слов
m_colors = c("#052F5F", "#114B5F", "#F45B69", '#6B2737')
b_background = c('#E4FDE1')
wordcloud2(PROPN,
           color = rep_len(m_colors, nrow(PROPN)),
           backgroundColor = b_background,
           size = 0.5)

#Частеречная разметка текста
ggplot(data = markup, aes(x = reorder(upos, n), y = n, fill = upos)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  guides(fill = FALSE) +
  labs(x = 'часть речи', y = 'всего') +
  ggtitle('Частеречная разметка текста')


