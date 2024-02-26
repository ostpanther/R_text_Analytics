setwd("/Users/gleb/Desktop/букваR")
library(languageR)
library(dplyr)
library(tidytext)
library(RColorBrewer)
library(grDevices) 
library(wordcloud2) 
library(wordcloud) 
library(extrafont)


# вектор с "Алисой"
alice <- tolower(alice)

# частотности для словa
freq <- as_tibble(table(alice)) %>% 
  rename(word = alice)

# удалить стоп-слова
freq_tidy <- freq %>% 
  anti_join(stop_words) 
# возможно, вы захотите произвести и другие преобразования
freq_tidy <- freq_tidy %>% 
  top_n(100)

m_colors = c("#19535F", "#FFE66D", "#D9FFF8")
b_background = "#797270"
# облако можно строить в любой библиотеке
wordcloud2(freq_tidy,
           color = rep_len(m_colors, nrow(freq_tidy)),
           backgroundColor = b_background,
           shape = 'triangle',
           size = 1)