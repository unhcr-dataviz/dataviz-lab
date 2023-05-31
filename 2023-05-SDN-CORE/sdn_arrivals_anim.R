# Sudan CORE | Animation of daily population figures
# Data source: ODP Sudan Situation

# load packages -----------------------------------------------------------
# pak::pkg_install("gganimate@1.0.7")
# error with gganimate 1.0.8 so install older version
library(tidyverse)
library(scales)
library(gganimate)
library(ragg)
library(unhcrthemes)



# load data ---------------------------------------------------------------

df_raw <- read_csv("https://docs.google.com/spreadsheets/d/1dE3sADZOC5V_N5dnBDGjnafVEx8BvzXz3QAGfODwYfM/export?format=csv&id=1dE3sADZOC5V_N5dnBDGjnafVEx8BvzXz3QAGfODwYfM")

df_raw



# wrangle data ------------------------------------------------------------

df_plot <-
  df_raw |>
  mutate(Date = paste0(Date, "-2023"),
         date = lubridate::dmy(Date)) |>
  select(-Date) |>
  rename(Total = TOTAL) |>
  pivot_longer(cols = Ethiopia:Total,
               names_to = "country",
               values_to = "pop_day")

levels <- df_plot |>
  filter(date == max(date)) |>
  arrange(pop_day) |>
  pull(country)



# plot unhcr --------------------------------------------------------------

theme_plot <- df_plot |>
  mutate(country = factor(country,
                          levels = levels)) |>
  ggplot(aes(x = date,
             y = pop_day,
             color = country)) +
  geom_line(linewidth = 0.75) +
  geom_point() +
  labs(title = "New arrivals from Sudan",
       caption = "Source: UNHCR (as of 16 May 2023)") +
  unhcrthemes::scale_color_unhcr_d(nmax = 9, order = c(3, 1, 2, 5, 7, 9)) +
  scale_y_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale()),
    position = "right",
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, 225e3, 25e3)
  ) +
  scale_x_date(date_breaks = "3 days", date_labels = "%d %b",
               expand = expansion(mult = 0.01)) +
  unhcrthemes::theme_unhcr(font_size = 13, grid = "Y", axis_title = FALSE) +
  guides(colour = guide_legend(nrow = 1))

theme_anim <- theme_plot +
  transition_reveal(date)

anim <- animate(plot = theme_anim, duration = 10, fps = 30, end_pause = 90,
                width = 1200, height = 1200*9/16, res = 150, device = "ragg_png",
                renderer = gifski_renderer())


# save --------------------------------------------------------------------

save_animation(animation = anim,
               file = "sdn_arrivals_2023-05-16.gif")






