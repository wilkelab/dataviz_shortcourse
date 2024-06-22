# data from:
# https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-month
# weather station: AUSTIN CAMP MABRY, TX US USW00013958
# temperature and precipitation data

library(tidyverse)
library(here)

atx_temps <- read_delim(
  here("datasets", "atx_temps_USW00013958.csv")
) |>
  select(date = DATE, temp_ave = TAVG, temp_max = TMAX, temp_min = TMIN, precip = PRCP) |>
  separate_wider_delim(date, delim = "-", names = c("year", "month")) |>
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    date_dec = year + (month-1)/12
  )

write_csv(atx_temps, here("datasets", "atx_temps.csv"))


start <- 1950
end <- 2010
temps_reduced <- filter(atx_temps, year >= start & year <= end)


# convert to time series object
response <- temps_reduced$temp_ave
temps_ts <- ts(
  data = response,
  start = start,
  end = c(end, 12),
  frequency = 12      # we have 12 time points per year
)


temps_stl <- stl(temps_ts, s.window = 7)

temps_detrended <- tibble(
  date_dec = temps_reduced$date_dec,
  response,
  seasonal = t(temps_stl$time.series)[1, ],
  trend = t(temps_stl$time.series)[2, ],
  remainder = t(temps_stl$time.series)[3, ]
)

# plot
p1 <- ggplot(temps_detrended, aes(date_dec, response)) + geom_line()
p2 <- ggplot(temps_detrended, aes(date_dec, seasonal)) + geom_line()
p3 <- ggplot(temps_detrended, aes(date_dec, trend)) + geom_line()
p4 <- ggplot(temps_detrended, aes(date_dec, remainder)) + geom_line()

plot_grid(p1, p2, p3, p4, ncol = 1, align = 'v')
