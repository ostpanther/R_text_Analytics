# скачайте с сайта Британской библиотеки датасет о Гарри Поттере
y_url <- "https://www.bl.uk/bibliographic/downloads/HarryPotterResearcherFormat_202306_csv.zip"
dir.create("HP")
download.file(url = my_url, destfile = "HP/HP.zip")
# после этого перейдите в директорию с архивом и распакуйте его 
setwd("HP")
unzip("HP.zip")
# сохраните список всех файлов с расширением .csv, используя подходящую функцию из base R
my_files <- list.files('/Users/gleb/HP', pattern = "\\.csv$", full.names = TRUE)
print(my_files)
# напишите цикл, который:
# 1) прочитает все файлы из my_files, используя для этого функцию read_csv() из пакета readr
# (аргумент show_col_types установите на FALSE);
# 2) для каждого датасета выяснит количество рядов _без_ NA в столбце BNB Number;
# 3) разделит число таких рядов на общее число рядов;
# 4) вернет таблицу c четырьми столбцами: 
## - название файла (id), 
## - число рядов (total), 
## - число рядов без NA (complete), 
## - доля полных рядов (ratio)
library(readr)
my_df <- data.frame(id = my_files, 
                    total = rep(0, length(my_files)),
                    complete = rep(0, length(my_files)),
                    ratio = rep(0, length(my_files)))

for (i in 1:length(my_files)) {
  # Чтение файла с помощью read_csv()
  full_data <- readr::read_csv(my_files[i], show_col_types = FALSE)
  # Кол-во рядов без NA в столбце "BNB Number"
  complete <- sum(!is.na(full_data$'BNB Number'))
  # Общие числа рядов
  total_rows <- nrow(full_data)
  # Вычисление общих чисел рядов
  ratio <- complete / total_rows
  # Запись результатов в таблицу
  my_df[i, "total"] <- total_rows
  my_df[i, "complete"] <- complete
  my_df[i, "ratio"] <- ratio
}