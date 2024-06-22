install.packages("mgcv")
install.packages("gratia")

library(tidyverse)
library(mgcv)
library(gratia)
library(gganimate)


blue_jays <- read_csv("https://wilkelab.org/SDS375/datasets/blue_jays.csv")

df <- blue_jays |>
  filter(sex == "M")

fit <- gam(head_length_mm ~ body_mass_g, data = df, method = "REML")

new_df <- tibble(
    body_mass_g = seq(from = 55, to = 85, length.out = 10)
  ) |>
  mutate(.row = row_number()) # needed to join in fitted samples

# fitted_values returns mean and confidence band, as usual
fv <- fitted_values(fit, data = new_df)

ggplot(df, aes(body_mass_g)) +
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    alpha = 0.2
  ) +
  geom_point(aes(y = head_length_mm), color = "navy") +
  geom_line(data = fv, aes(y = .fitted))

fs <- fitted_samples(fit, data = new_df, n = 30, seed = 10) |> 
  left_join(new_df, by = ".row")

p <- ggplot(df, aes(body_mass_g)) +
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    alpha = 0.2
  ) +
  geom_point(aes(y = head_length_mm), color = "navy") +
  geom_line(
    data = fs,
    aes(y = .fitted, group = .draw),
    linewidth = 0.2
  )
p

p +
  transition_states(.draw)

cars93 <- read_csv("https://wilkelab.org/SDS375/datasets/cars93.csv") |>
  select(capacity = Fuel.tank.capacity, price = Price)
cars93

# default geom_smooth() formula
fit <- gam(capacity ~ s(price, bs = "cs"), data = cars93, method = "REML")

new_df <- tibble(
  price = seq(from = 5, to = 65, length.out = 100)
) |>
  mutate(.row = row_number()) # needed to join in fitted samples

# fitted_values returns mean and confidence band, as usual
fv <- fitted_values(fit, data = new_df)

ggplot(cars93, aes(price)) +
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    alpha = 0.2
  ) +
  geom_point(aes(y = capacity), color = "navy") +
  geom_line(data = fv, aes(y = .fitted))

fs <- fitted_samples(fit, data = new_df, n = 30, seed = 10) |> 
  left_join(new_df, by = ".row")

p <- ggplot(cars93, aes(price)) +
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    alpha = 0.2
  ) +
  geom_point(aes(y = capacity), color = "navy") +
  geom_line(
    data = fs,
    aes(y = .fitted, group = .draw),
    linewidth = 0.2
  )
p

p +
  transition_states(.draw)
