# Top 15 refugee crisis by COO and COA | Animation of top 15 refugee crisis from 2012 to 2022 for both country of origin and asylum
# Data source: RDF

# load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(gganimate)
library(ragg)
library(unhcrthemes)
library(gifski)

# Theme
theme_set(theme_void(base_size = 18, base_family = "Lato"))
theme_update(
  panel.background = element_rect(fill = "white", color = "white"),
  plot.background = element_rect(fill = "white", color = "white"),
  plot.margin = margin(0, 0, 0, 0),
  plot.caption = element_text(hjust = 0, color = "grey40",
                              lineheight = 1.2),
  plot.caption.position = "plot",
  legend.position = "none",
)


# load data ---------------------------------------------------------------

df_ref <- readr::read_csv(here::here("ref_oip_2002_2022.csv"))



# wrangle data ------------------------------------------------------------

df_ref_long <-
  df_ref |>
  pivot_longer(cols = c("ref", "oip"),
               names_to = "pop_type",
               values_to = "pop_num") |>
  drop_na(pop_num) |>
  filter(coo != "Unknown") |> # remove Unknown origin
  mutate(
    coo = case_when(
      coo == "Serbia and Kosovo: S/RES/1244 (1999)" ~ "Serbia*",
      coo == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      TRUE ~ coo),
    coa = case_when(
      coa == "Serbia and Kosovo: S/RES/1244 (1999)" ~ "Serbia*",
      coa == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      coa == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
      TRUE ~ coa)
  ) # clean long labels

threshold <- 15

df_coo <-
  df_ref_long |>
  group_by(year, coo) |>
  summarise(pop_tot = sum(pop_num, na.rm = TRUE)) |>
  ungroup() |>
  group_by(year) |>
  mutate(rank = rank(-pop_tot) * 1) |>
  filter(rank <= threshold) |>
  ungroup()

df_coa <-
  df_ref_long |>
  group_by(year, coa) |>
  summarise(pop_tot = sum(pop_num, na.rm = TRUE)) |>
  ungroup() |>
  group_by(year) |>
  mutate(rank = rank(-pop_tot) * 1) |>
  filter(rank <= threshold) |>
  ungroup()



# plot  --------------------------------------------------------------

# camcorder
# camcorder::gg_record(
#   dir = here::here("R", "202211_ref_barchart_race", "dev"),
#   #scale = 2,
#   device = "png",
#   width = 1800,
#   height = 1200,
#   units = "px"
# )

# coo plot
p_coo <-
  df_coo |>
  #filter(year == 2022) |>
  ggplot(aes(y = rank)) +
  geom_tile(aes(x = pop_tot/2,
                width = pop_tot,
                fill = rank),
            height = 0.8,
            #fill = unhcr_pal(n = 1, name = "pal_blue"), # fixed fill
            color = NA) +
  geom_text(aes(x = 0,
                label = coo),
            hjust = 1,
            nudge_x = -5e4,
            size = 18/.pt,
            color = "grey10") +
  geom_text(aes(x = pop_tot,
                label = paste0(" ", round(pop_tot / 1e6, digits = 2), "M")), # add empty string to fix label issue
            hjust = 0,
            nudge_x = 3e4,
            size = 18/.pt,
            color = "grey10") +
  coord_cartesian(clip = "off") +
  scale_y_reverse() +
  scale_fill_unhcr_c( direction = -1) +
  labs(tag = "{closest_state}",
       title = "Refugees by country of origin | 2002-2022",
       caption = "The 15 countries shown represent those producing the largest numbers of refugees, people in refugee like situations and other people in need of international protection\nat the end of at least one of the years during the last two decades. *Serbia and Kosovo: S/RES/1244 (1999)\nSource: UNHCR Data Finder as of June 2022") +
  theme(plot.margin = margin(10, 90, 10, 240),
        plot.tag = element_text(size = 72, face = "bold",
                                color = unhcr_pal(n = 1, name = "pal_blue")),
        plot.tag.position = c(.9, .12),
        plot.title = element_text(face = "bold", size = 36)
  )

# coa plots
p_coa <-
  df_coa |>
  #filter(year == 2022) |>
  ggplot(aes(y = rank)) +
  geom_tile(aes(x = pop_tot/2,
                width = pop_tot,
                fill = rank),
            height = 0.8,
            #fill = unhcr_pal(n = 1, name = "pal_blue"), # fixed fill
            color = NA) +
  geom_text(aes(x = 0,
                label = coa),
            hjust = 1,
            nudge_x = -5e4,
            size = 18/.pt,
            color = "grey10") +
  geom_text(aes(x = pop_tot,
                label = paste0(" ", round(pop_tot / 1e6, digits = 2), "M")), # add empty string to fix label issue
            hjust = 0,
            nudge_x = 3e4,
            size = 18/.pt,
            color = "grey10") +
  coord_cartesian(clip = "off") +
  scale_y_reverse() +
  scale_fill_unhcr_c( direction = -1) +
  labs(tag = "{closest_state}",
       title = "Refugees by country of asylum | 2002-2022",
       caption = "The 15 countries shown represent those hosting the largest numbers of refugees, people in refugee like situations and other people in need of international protection\nat the end of at least one of the years during the last two decades. *Serbia and Kosovo: S/RES/1244 (1999)\nSource: UNHCR Data Finder as of June 2022") +
  theme(plot.margin = margin(10, 90, 10, 240),
        plot.tag = element_text(size = 72, face = "bold",
                                color = unhcr_pal(n = 1, name = "pal_blue")),
        plot.tag.position = c(.9, .12),
        plot.title = element_text(face = "bold", size = 36)
  )



# animation ---------------------------------------------------------------

# coo anim
a_coo <-
  p_coo +
  transition_states(year, wrap = FALSE,
                    transition_length = 1, state_length = 2) +
  ease_aes('cubic-in-out')

anim_coo <- animate(a_coo, start_pause = 25, end_pause = 50,
                    nframes = 600, fps = 15,
                    height = 1200, width = 1800, device = "ragg_png")

# coa anim
a_coa <-
  p_coa +
  transition_states(year, wrap = FALSE,
                    transition_length = 1, state_length = 2) +
  ease_aes('cubic-in-out')

anim_coa <- animate(a_coa, start_pause = 25, end_pause = 50,
                    nframes = 1000, fps = 25,
                    height = 1200, width = 1800, device = "ragg_png")


# save --------------------------------------------------------------------

save_animation(animation = anim_coo,
               file = here::here("ref_top15_coo_2022-11.gif"))

save_animation(animation = anim_coa,
               file = here::here("ref_top15_coa_2022-11.gif"))



