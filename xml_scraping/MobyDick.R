library(XML)
library(purrr)
library(dplyr)
library(stringr)
library(tidyverse)
library(tidyr)
library(numbers)
library(tibble)

# ЗАДАНИЕ 1
# в репозитории вы найдете файл MobyDick.xml
# его надо прочитать и сделать тиббл с тремя столбцами:
# chapter (например, CHAPTER I); title (например, LOOMINGS); text (полностью текст главы)

#читаем файл
doc <- xmlTreeParse("MobyDick.xml", useInternalNodes = T)
rootnode <- xmlRoot(doc)

#пространство имен
ns <- xmlNamespace(rootnode)

#извлекаем все p
p_nodes <- getNodeSet(rootnode, "//tei:body//tei:p", namespaces = c(tei = ns))

# ф-ция для получения chapter
get_chapter <- function(node) {
  if (!is.na(node)) {
    list_of_chapter <- xmlElementsByTagName(xmlParent(node), "head")
    chapter <- xmlValue(xmlParent(node)[["head"]])
  } 
  else {
    list_of_chapter <- xmlElementsByTagName(xmlParent(xmlParent(node)), "head")
    chapter <- xmlValue(xmlParent(node)[["head"]])
  }
  return(chapter)
}

# ф-ция для получения title
get_title <- function(node) {
  if (!is.na(node)) {
    list_of_title <- xmlElementsByTagName(xmlParent(node), "head")
    title <- xmlValue(xmlParent(node)[[2]])
  } 
  else {
    list_of_title <- xmlElementsByTagName(xmlParent(xmlParent(node)), "head")
    title <- xmlValue(xmlParent(node)[[2]])
  }
  return(title)
}

#на все узлы
chapter <- map(p_nodes, get_chapter)
title <- map(p_nodes, get_title)

# достать текст построчно
text <- map_chr(p_nodes, xmlValue)

#сшиваем все в одну таблицу
MobyDick <- tibble(chapter, title, text)


# ЗАДАНИЕ 2
# преобразуйте title, чтобы там были только строчные буквы
# удалите слово chapter из столбца chapter
MobyDick$chapter <-  gsub('CHAPTER', '', MobyDick$chapter )
MobyDick <- MobyDick %>% 
  mutate(title = tolower(title))

# удалите пунктуацию в chapter и title (используйте separate)
MobyDick <- separate(MobyDick, title, into = c("title", "title2"), sep = "\\.")
MobyDick <- separate(MobyDick, chapter, into = c("chapter", "chapter2"), sep = "\\.")

MobyDick <- subset(MobyDick, select = - c(title2, chapter2))

# ЗАДАНИЕ 3
# в стоблце chapter замените римские номера глав на арабские :)

#не получилось:(
#replace_roman <- function(x) {
#  if (!is.na(x)) {
#    if (is.roman(x)) {
#      return(as.character(roman_to_arabic(x)))
#    }
#  }
#  return(x)
#}

#df_arab <- MobyDick %>% mutate(chapter = replace_roman(chapter))





