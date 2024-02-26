#2) Посчитайте TTR для каждой сказки (целиком). Лемматизировать не надо. 
#Найдите сказку с самым высоким TTR. Название запишите в файле. (Оценка 0/1)

library(hcandersenr)
library(tokenizers)
library(tidytext)
library(udpipe)
library(dplyr)
library(languageR)

#получим тексты сказок Ханса Кристиана Андерсена
tales <- hcandersenr::hcandersen_en

#токенизация
tales_token <- tales %>% 
   unnest_tokens(word, text)

#сколько всего сказок(156)
unique_values <- unique(tales$book)

#преобразование в вектор с помощью цикла
results <- list()
for(book_name in unique(tales_token$book)) {
  # Применяем код к каждой книге как из лекции
  words <- tales_token %>% filter(book == book_name) %>% pull(word)
  results[[book_name]] <- words
}

#в переменной results_ttr у нас будет таблица с названием сказки и соответствующим TTR
results_ttr <- data.frame(book = unique(tales_token$book), TTR = numeric(length(unique(tales_token$book))))

#Пройдемся циклом по всем сказкам
#сначала посчитаем количество уникальных слов (types) и общее количество слов (tokens) в каждой сказке
#Затем вычислим TTR для каждой сказки, разделив количество уникальных слов на общее количество слов 
for (i in 1:length(unique(tales_token$book))) {
  book_name <- unique(tales_token$book)[i]
  words <- results[[book_name]]
  types <- length(unique(words))
  tokens <- length(words)
  ttr <- types / tokens
  results_ttr[i, ] <- c(book_name, ttr)
}

max_ttr <- max(results_ttr$TTR)
most_diverse_tale <- results_ttr[which(results_ttr$TTR == max_ttr), "book"]

most_diverse_tale

# сказка с самым высоким TTR. - "A picture from the ramparts" 