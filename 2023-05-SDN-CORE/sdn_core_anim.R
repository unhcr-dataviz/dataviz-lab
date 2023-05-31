# Sudan CORE | Animation of daily population figures
# Data source: ODP Sudan Situation


# load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(gganimate)
library(systemfonts)
library(ragg)


# load data ---------------------------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/1dE3sADZOC5V_N5dnBDGjnafVEx8BvzXz3QAGfODwYfM/export?format=csv&id=1dE3sADZOC5V_N5dnBDGjnafVEx8BvzXz3QAGfODwYfM"

df_raw <- read_csv(url)


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


# plot --------------------------------------------------------------------
# record
# camcorder::gg_record(
#   device = agg_png,
#   dir = here::here("temp-plot"),
#   width = 1200/300,
#   height = 675/300,
#   dpi = 300,
#   units = "in"
# )

# plot
plot <- 
  df_plot |> 
  mutate(country = factor(country,
                          levels = levels)) |> 
  ggplot(aes(x = date,
             y = pop_day)) +
  geom_line(aes(color = country,
             group = country),
             linewidth = .35) +
  labs(title = "New arrivals from Sudan since 16 April 2023",
       caption = "Source: UNHCR",
       y = NULL,
       x = NULL) +
  unhcrthemes::scale_color_unhcr_d(nmax = 9, order = c(3, 1, 2, 5, 7, 9)) +
  scale_y_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale()),
    #expand = c(0.02, 0.1), 
    position = "right"
    ) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  theme_minimal(base_family = "Lato", base_size = 15,
    base_line_size = .25, base_rect_size = .25) +
  theme(
    text = element_text(family = "Lato", color = "grey10"),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(seq(10, 4)),
    legend.position = "top",
    legend.title = element_blank(),
    legend.margin = margin(t = 0, l = 0, b = 0, r = 0),
    legend.justification = "left",
    legend.key.size = unit(8, "pt"),
    legend.text = element_text(size = 12),
    plot.title = element_text(family = "Lato", size = 22, face = "bold",
    color = "black", margin = margin(seq(0, 4))), 
    plot.title.position = "plot", 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y.right = element_text(margin = margin(l = 0)),
    plot.caption = element_text(size = 10, color = "grey40",
    hjust = 0), 
    plot.caption.position = "plot"
  ) +
  guides(colour = guide_legend(nrow = 1))


# animate --------------------------------------------------------------------

plot_anim <- plot +
  transition_reveal(along = date)

anim <- animate(
  plot_anim, 
  fps = 30,
  duration = 8,
  device = "ragg_png",
  end_pause = 90,
  width = 1200,
  height = 675,
  res = 300)


# save --------------------------------------------------------------------
path <- "2023-05-SDN-CORE"

save_animation(animation = anim, 
               file = here::here(path, "sdn_daily_arrivals.gif"))

