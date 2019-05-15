# Вариант 4 - для региона 73 (Ульяновск) рассчитайте урожайность пшеницы в 2003 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 9 лет, 
# с метеостанций на расстоянии от 70 до 210 км
# 54.311664, 48.390151

# Подключим библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# station_data = ghcnd_stations()
# write.csv(station_data, "station_data.csv")
station_data = read.csv("station_data.csv")
# Получим список метеостанций
ulyanovsk = data.frame(id = "ULYANOVSK", latitude = 54.311664,  longitude = 48.390151)
#найдем станции, соответствующие критериям
ulyanovsk_around = meteo_nearby_stations(lat_lon_df = ulyanovsk, station_data = station_data,
                                         radius = 210, var = "TAVG", 
                                         year_min = 1994, year_max = 2002)
#уберем первую станцию (она находится ближе, чем 70 км)
ulyanovsk_around$ULYANOVSK = ulyanovsk_around$ULYANOVSK[-1,]
#создадим таблицу
all_data = tibble()
#запишем в таблицу данные 
for (i in 2:length(ulyanovsk_around$ULYANOVSK[,1]))
{
  # Определим станцию:
  ulyanovsk_id = ulyanovsk_around[["ULYANOVSK"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = ulyanovsk_id,
                          var="TAVG",
                          date_min="1994-01-01",
                          date_max="2002-12-31")
  #объединим данные в таблице
  all_data = bind_rows(all_data, data %>%
                         #добавим колонки для группировки по году и месяцу
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         #найдем суммарную среднюю активную температуру по месяцу за каждый год для станции
                         summarise (tavg = sum(tavg[tavg>50])/10)
  )
}

# Изменения в таблице сохранятся в векторе clean_data.
clean_data = all_data %>%
  # Добавим колонку month для группировки данных:
  group_by(month) %>%
  # Найдем месячный d и cумму активных тмператур для каждой станции:
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  #добавим данные из таблицы с показателем d
  # Добавим колонки для расчета:
  mutate (a = af, b = bf, d = df) %>%
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#Согласно расчету, урожайность пшеницы в Ульяновской области в 2002 году составила (ц/га):
Yield = sum(clean_data$fert); Yield
