# data from:
# https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-month
# weather station: AUSTIN CAMP MABRY, TX US USW00013958
# temperature and precipitation data

atx_temps <- read_delim(
  "~/Desktop/USW00013958.csv"
) |>
  select(date = DATE, temp_ave = TAVG, temp_max = TMAX, temp_min = TMIN, precip = PRCP) |>
  separate_wider_delim(date, delim = "-", names = c("year", "month")) |>
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    date_dec = year + (month-1)/12
  )


start <- 2000
end <- 2010
temps_reduced <- filter(atx_temps, year >= start & year <= end)


# convert to time series object
data <- temps_reduced$temp_ave
temps_ts <- ts(
  data = data,
  start = start,
  end = c(end, 12),
  frequency = 12      # we have 12 time points per year
)


temps_stl <- stl(temps_ts, s.window = 7)

temps_detrended <- tibble(
  date_dec = temps_reduced$date_dec,
  data,
  seasonal = t(temps_stl$time.series)[1, ],
  trend = t(temps_stl$time.series)[2, ],
  remainder = t(temps_stl$time.series)[3, ]
)

# plot
p1 <- ggplot(temps_detrended, aes(date_dec, data)) + geom_line()
p2 <- ggplot(temps_detrended, aes(date_dec, seasonal)) + geom_line()
p3 <- ggplot(temps_detrended, aes(date_dec, trend)) + geom_line()
p4 <- ggplot(temps_detrended, aes(date_dec, remainder)) + geom_line()

plot_grid(p1, p2, p3, p4, ncol = 1, align = 'v')
