library(gutenbergr)
library(dplyr)
library(tidyr)

works <- gutenberg_works()

# Отберите ряды, в которых gutenberg_author_id равен 65;
# после этого выберите два столбца: author, title

my_data <- works %>%
  filter(gutenberg_author_id == '65') %>%
  select(author, title)

# Загрузите данные об авторах и выберите столбцы: author, deathdate

authors <- gutenberg_authors %>%
  select(author, deathdate)

# Соедините my_data с данными о смерти автора из authors, 
# так чтобы к my_data добавился новый столбец. 

# После этого используйте функцию separate, 
# чтобы разделить столбец с именем и фамилией на два новых: author, name.

# Удалите столбец с именем автора, оставив только фамилию.

# Добавьте новый столбец century, 
# используя функцию mutate и данные из столбца deathdate. 
# Используйте оператор pipe, не сохраняйте промежуточные результаты!

my_data <- my_data %>% 
  left_join(authors, deathdate, by = "author") %>% 
  separate(author, into = c('author', 'name')) %>% 
  select(-name) %>%
  mutate(century = floor(deathdate/100 + 1)) %>%
  print