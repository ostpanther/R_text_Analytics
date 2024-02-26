library(tidyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(ggplot2)
library(lubridate)

# прочитайте файлы json_laureates.json и json_awards.json (в репозитории)
# не передавайте функции fromJSON никакие аргументы, кроме пути к файлу

#читаем файлы формата JSON
path1 <- "/Users/gleb/Desktop/R/files for HW/json_award.json"
path2 <- "/Users/gleb/Desktop/R/files for HW/json_laureates.json"
awards <- fromJSON(txt = path1)
laureates <- fromJSON(txt = path2)


# ЗАДАНИЕ 1
# используйте unnest(), чтобы "распаковать" столбец laureates в awards
# для починки имен передавайте unnest аргумент names_repair = "minimal"
# используйте jsonlite::flatten(), чтобы избавиться от вложенных датафреймов
# используйте unnest(), чтобы "распаковать" столбец nobelPrizes в laureates
# также используйте unnest(), чтобы "распаковать" столбец affiliations 
#(может снова понадобиться flatten())


awards <- awards %>%
  unnest(laureates, names_repair = "minimal") %>%
  flatten() 

laureates <- laureates %>%
  unnest(nobelPrizes, names_repair = "minimal") %>%
  flatten() 

laureates <- laureates %>%
  unnest(affiliations, names_repair = "minimal") %>%
  flatten()


# ЗАДАНИЕ 2
# удалите столбцы, которые начинаются с links, используя комбинацию select() и starts_with()
# объедините тибблы, используя left_join; 
# переименуйте столбцы name.en, country.en, category.en (убрать .en)


awards <- awards %>% 
  select(-starts_with('links'))

laureates <- laureates %>% 
  select(-starts_with('links'))

joined_data <- left_join(awards, laureates) %>% 
  rename(name = name.en,
         country = country.en,
         category = category.en)


# в каких университетах больше всего ноб. лауреатов? 
# University of California(34), Harvard University(28)

#создал таблицу для страны и университета и отобрал топ-18 
university_and_country <- joined_data %>% 
  select(name,country) %>% 
  na.omit() %>%  
  add_count(name) %>% 
  arrange(desc(n)) %>% 
  distinct() %>% 
  top_n(18, n)


# посчитайте, отберите топ-n и создайте диаграмму, цветом закодировав страну;
# график экспортируйте с разумными настройками 
# и ДОБАВЬТЕ В РЕПОЗИТОРИЙ в формате png


ggplot(data = university_and_country, aes(name, n, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(x = 'Alma Mater', y = 'Number of laureates') +
  ggtitle("Plot by Gleb L")

ggsave (filename = "top Alma Matters Gleb.png",
        plot = last_plot(),
        device = "png",
        dpi = 300)


# ЗАДАНИЕ 3
# отберите в отдельный тиббл столбцы awardYear, dateAwarded, birth.date
# там, где нет данных в столбце dateAwarded, считайте за дату вручения 10 октября и данные из столбца awardYear 
# (для преобразования dateAwarded понадобятся mutate, paste, case_when)
# после этого преобразуйте birth.date и dateAwarded в даты, используя подходящие функции из lubridate
# добавьте новый столбец age, куда сохраните возраст на момент вручения
# понадобятся функции interval(), years() из lubridate и деление без остатка :)


new_tibble <- joined_data %>% 
  #выбрал определенные столбцы из joined_data 
  select(awardYear, dateAwarded, birth.date) %>% 
  #где нет данных в столбце dateAwarded, посчитал за дату вручения 10 октября и данные из столбца awardYear 
  mutate(dateAwarded = case_when(is.na(dateAwarded) ~ paste0(awardYear, "-10-09"), 
                                 TRUE ~ dateAwarded)) %>% 
  #преобразовал данные в birth.date, dateAwarded в формат Date
  mutate(birth.date = as.Date(birth.date),
         dateAwarded = as.Date(dateAwarded)) %>% 
  #добавил столбец age, посчитал разницу в возрасте с помощью interval() и делил целочисленно на years(1), чтобы получить возраст в годах
  mutate(age = interval(birth.date, dateAwarded) %/% years(1))


# дальше постройте _любую_ информативную (!) диаграмму, которая показывает, как меняется возраст кандидатов в зависимости от года вручения
# убедитесь, что данные в awardYear нужного типа, при необходимости преобразуйте
# график тоже экспортируйте в png, подобрав подходящие настройки и ЗАГРУЗИТЕ В РЕПОЗИТОРИЙ

#преобразовал awardYear в формат Date 
new_tibble <- new_tibble %>%
  mutate(awardYear = as.Date(awardYear, format = "%Y")) %>%
  mutate(awardYear = format(awardYear, "%Y")) %>% 
  na.omit() 

#создал новую таблицу в которой отображается средний возраст кандидатов по каждому году 
my_plot <- new_tibble %>% 
  group_by(awardYear) %>%
  summarize(mean_age = floor(mean(age)))

#строю диаграмму 
ggplot(my_plot, aes(awardYear,mean_age)) +
  geom_point(shape = 21, colour = "red", fill = "gold", size = 2) +
  #изменил размерность x-axis чтобы года не наслаивалиьс друг на друга
  scale_x_discrete(breaks = seq(1901, 2020, by = 5)) +
  labs(x = "Год вручения премии", y = "Средний возраст кандидата") +
  ggtitle("Изменение возраста кандидатов в зависимости от года вручения Нобелевской премии (Gleb L)")
#можно наблюдать как меняется возраст врученя нобелевской премии в зависимости от возраста 
#так, например, отсутвуют кандидаты до 50 лет после 68 года; поднялся возраст кандидатов, получающих Нобелевскую премию
# а также видно, что до 1952 года почти нет представителей с возрастом более 59 лет

ggsave (filename = "Изменение возраста кандидатов в зависимости от года вручения Нобелевской премии (Gleb L).png",
        plot = last_plot(),
        device = "png",
        dpi = 300)

