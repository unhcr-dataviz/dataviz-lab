# Top 10 IDP crisis of last decade | Animation of top 10 IDP crisis from 2010 to 2020
# Data source: RDF

# load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(gganimate)
library(ragg)
library(unhcrthemes)



# load data ---------------------------------------------------------------

idp_raw <- read_csv(here::here("idp_2010_2020.csv"))



# wrangle data ------------------------------------------------------------

idp_rank <- idp_raw |>
  group_by(year) |>
  mutate(rank = rank(-total_idp, ties.method = "first"),
         idp_label = paste0(" ", round(total_idp / 1000000, digits = 2), "M"),
         country_label = stringr::str_wrap(country, width = 18)) |>
  filter(rank <= 10)



# plot unhcr --------------------------------------------------------------

idp_plot <- ggplot(
  data = idp_rank,
  aes(
    y = rank,
    group = country
  )
) +
  geom_tile(aes(
    x = total_idp / 2,
    width = total_idp
  ),
  height = 0.8,
  alpha = 0.7,
  fill = unhcr_pal(n = 1, "pal_blue"),
  color = NA
  ) +
  geom_text(aes(
    x = 0,
    label = country_label
  ),
  hjust = 1,
  nudge_x = -50000,
  size = 14 / .pt
  ) +
  geom_text(aes(
    x = total_idp,
    label = idp_label
  ),
  hjust = 0,
  nudge_x = 50000,
  size = 14 / .pt
  ) +
  coord_cartesian(clip = "off") +
  scale_y_reverse() +
  scale_x_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale())) +
  labs(
    title = "Top 10 IDP crises of the last decade",
    subtitle = "{closest_state}",
    caption = "Source: UNHCR Data Finder",
    x = "Total number of IDPs",
    y = NULL
  ) +
  theme_unhcr(
    font_size = 16,
    axis = FALSE,
    axis_text = "x",
    grid = "Xx"
  ) +
  theme(
    plot.margin = margin(80, 120, 80, 120),
    plot.subtitle = element_text(
      face = "bold",
      color = unhcr_pal(n = 1, "pal_blue")
    )
  ) +
  transition_states(
    states = year,
    wrap = FALSE,
    transition_length = 1,
    state_length = 2
  ) +
  ease_aes('cubic-in-out')

# Create animation
idp_anim <- animate(
  plot = idp_plot,
  nframes = 250, fps = 25,
  width = 1200, height = 1000,
  end_pause = 15
)


# save --------------------------------------------------------------------

anim_save(here::here("idp_top10_2020-11.gif"))



