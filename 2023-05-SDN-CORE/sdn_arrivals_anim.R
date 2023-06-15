# Sudan CORE | Animation of daily population figures
# Data source: ODP Sudan Situation

# load packages -----------------------------------------------------------
library(tidyverse)
library(scales)
# error with gganimate 1.0.8 so install older version
# if {pak} not installed first run:
# install.packages('pak')
# install gganimate 1.0.7:
# pak::pkg_install("gganimate@1.0.7")
# if error from pak try remotes package
# remotes::install_version("gganimate", version = "1.0.7")
library(gganimate)
library(ragg)
library(unhcrthemes)



# load data ---------------------------------------------------------------

df_raw <- read_csv("https://docs.google.com/spreadsheets/d/1dE3sADZOC5V_N5dnBDGjnafVEx8BvzXz3QAGfODwYfM/export?format=csv&id=1dE3sADZOC5V_N5dnBDGjnafVEx8BvzXz3QAGfODwYfM")

# df_raw


# wrangle data ------------------------------------------------------------

# data for plot
df_plot <-
  df_raw |>
  mutate(Date = paste0(Date, "-2023"),
         date = lubridate::dmy(Date)) |>
  select(-Date) |>
  rename(Total = TOTAL) |>
  pivot_longer(cols = Ethiopia:Total,
               names_to = "country",
               values_to = "pop_day")

# last entry order by pop
levels <- df_plot |>
  filter(date == max(date)) |>
  arrange(pop_day) |>
  pull(country)

# last date label for file name
last_date <-
  df_plot |>
  filter(date == max(date)) |>
  distinct(date) |>
  pull(date)

# caption including last date
caption <-
  paste0("Source: UNHCR (", format(last_date,  '%d %B %Y'), ")")


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
       caption = caption) +
  unhcrthemes::scale_color_unhcr_d(nmax = 9, order = c(3, 1, 2, 5, 7, 9)) +
  scale_y_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale()),
    position = "right",
    expand = expansion(mult = c(0, 0.05)),
    limits = c(NA, 500e3),
    # adjust breaks if needed
    # breaks = seq(0, 500e3, 100e3)
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
               file = paste0("sdn_arrivals_", last_date, ".gif"))






