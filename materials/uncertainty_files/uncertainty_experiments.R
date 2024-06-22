install.packages("mgcv")
install.packages("gratia")
install.packages("gifski")

library(tidyverse)
library(mgcv)
library(gratia)
library(gganimate)


blue_jays <- read_csv("https://wilkelab.org/SDS375/datasets/blue_jays.csv")

df <- blue_jays |>
  filter(sex == "M")

fit <- gam(head_length_mm ~ body_mass_g, data = df, method = "REML")

new_df <- tibble(
    body_mass_g = seq(from = 59, to = 82, length.out = 10)
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

blue_jays_male <- blue_jays |>
  filter(sex == "M")

ggplot(blue_jays_male, aes(body_mass_g)) + 
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    fill="grey70", color = NA, alpha = 1/2
  ) +
  geom_point(aes(y = head_length_mm), color = "grey60", size = 1.5) +
  #geom_line(data = sample_df, aes(group = .draw), color = "#0072B2", linewidth = 0.3) +
  geom_line(
    data = fv, aes(y = .fitted),
    color = "#0072B2", linewidth = 1
  ) +
  scale_x_continuous(
    limits = c(59, 82),
    expand = c(0, 0),
    name = "body mass (g)") +
  scale_y_continuous(
    limits = c(52, 61),
    expand = c(0, 0),
    name = "head length (mm)"
  ) +
  theme_half_open()

ggplot(blue_jays_male, aes(body_mass_g)) + 
  geom_ribbon(
    data = fv, aes(ymin = .lower_ci, ymax = .upper_ci),
    fill="grey70", color = NA, alpha = 1/2
  ) +
  geom_point(aes(y = head_length_mm), color = "grey60", size = 1.5) +
  geom_line(
    data = fv, aes(y = .fitted),
    color = "grey70", linewidth = 1
  ) +
  geom_line(
    data = fs,
    aes(y = .fitted, group = .draw),
    color = "#0072B2", linewidth = 0.6
  ) +
  scale_x_continuous(
    limits = c(59, 82),
    expand = c(0, 0),
    name = "body mass (g)") +
  scale_y_continuous(
    limits = c(52, 61),
    expand = c(0, 0),
    name = "head length (mm)"
  ) +
  theme_half_open() +
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


animate(
  p + transition_states(.draw),
  nframes = 60,
  fps = 5,
  width = 600, height = 450,
  res = 100
)

anim_save(
  "~/Desktop/test.gif",
  p + transition_states(.draw),
  nframes = 60,
  fps = 5,
  width = 600, height = 450,
  res = 100
)

