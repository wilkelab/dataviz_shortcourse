# data from:
# https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-month
# weather station: AUSTIN CAMP MABRY, TX US USW00013958
# temperature and precipitation data

library(tidyverse)
library(here)

atx_temps <- read_csv(
  here("datasets", "atx_temps.csv"),
  show_col_types = FALSE
)

start <- 1970
end <- 2010
temps_reduced <- filter(atx_temps, year >= start & year <= end)
response <- temps_reduced$temp_ave
response <- temps_reduced$precip


# convert to time series object
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
temps_detrended |>
  pivot_longer(-date_dec, names_to = "component", values_to = "temperature") |>
  mutate(
    component = fct_relevel(component, "response", "trend", "seasonal", "remainder")
  ) |>
  ggplot(aes(date_dec, temperature)) +
  geom_line() +
  facet_wrap(~component, scales = "free_y", ncol = 1)
