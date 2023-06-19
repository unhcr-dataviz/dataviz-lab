# Global Trends 2022 | Chord diagram by region
# Data source: UNHCR Refugee Data Finder


# load packages -----------------------------------------------------------

library(tidyverse)
library(circlize)


# load data ---------------------------------------------------------------

df_flows <- readr::read_csv(here::here("2023-06-gt2022", "flows_all_region.csv"))



# wrangle data ------------------------------------------------------------

ori_label <- df_flows |>
  group_by(OriginRegion) |>
  summarise(pop = round(sum(Count)/1e6,1)) |>
  mutate(origin_label = paste0(OriginRegion, "\n", pop, "M")) |>
  select(-pop)

asy_label <- df_flows |>
  group_by(AsylumRegion) |>
  summarise(pop = round(sum(Count)/1e6,1)) |>
  mutate(asylum_label = paste0(AsylumRegion, "\n", pop, "M")) |>
  select(-pop)

df_plot <- df_flows |>
  left_join(ori_label, by = "OriginRegion") |>
  left_join(asy_label, by = "AsylumRegion") |>
  select(origin_label, asylum_label, Count)


# plot --------------------------------------------------------------------

# color palette
region_color <- rep(c(unhcrthemes::unhcr_pal(name = "pal_unhcr_region")[1],
                 unhcrthemes::unhcr_pal(name = "pal_unhcr_region")[4:7]),
                 each = 2)

color_name <- df_plot |>
  pivot_longer(cols = c("origin_label", "asylum_label"),
               values_to = "region") |>
  select(region) |>
  distinct(region) |>
  arrange(region) |>
  pull(region)

chord_color <- setNames(region_color, color_name)

# slice order
ori_order <- df_plot |>
  distinct(origin_label) |>
  arrange(origin_label) |>
  pull(origin_label)

asy_order <- df_plot |>
  distinct(asylum_label) |>
  arrange(desc(asylum_label)) |>
  pull(asylum_label)

chord_order <- c(ori_order, asy_order)

# parameters
circos.clear()
circos.par(start.degree = 90, clock.wise = FALSE)

# plot
chordDiagram(df_plot,
             order = chord_order,
             grid.col = chord_color,
             direction.type = c("arrows"),
             transparency = 0.5,
             directional = 1,
             link.arr.type = "big.arrow",
             link.arr.length = 0.05,
             link.sort = TRUE,
             annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.05, 0.025))

# extra base R plot stuff not included in SVG export
# abline(v = 0, lty = 2, col = "#00000080")
# title("From which region refugees are coming from and where are they going?")

