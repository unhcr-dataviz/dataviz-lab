# setup -------------------------------------------------------------------

# load packages
library(tidyverse)
library(refugees)
library(sf)
library(unhcrthemes)
library(ragg)

# vars
sf_use_s2(FALSE)
robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
pop_threshold <- 1000
poly_col <- "#EAE6E4"
line_col <- "#A8AAAD"



# data --------------------------------------------------------------------

# geodata
wrl_a_sf <- st_read(
  here::here("input", "wrl_boundaries.gpkg"),
  layer = "wrl_bnda_int_25m_ungis"
) |>
  filter(iso3cd != "ATA")

wrl_l_sf <- st_read(
  here::here("input", "wrl_boundaries.gpkg"),
  layer = "wrl_bndl_int_25m_ungis"
) |>
  filter(bdystyle != "donotshow")

wrl_p_df <- read_csv(here::here("input", "wrl_polbnd_int_1m_p_unhcr.csv")) |>
  select(iso3, gis_name, color_code, lon, lat, secondary_territory)


wrl_p_sf <- wrl_p_df |>
  group_by(iso3) |>
  mutate(n_iso = n()) |>
  ungroup() |>
  mutate(centroid = case_when(
    n_iso == 1 ~ TRUE,
    n_iso > 1 & secondary_territory == 0 ~ TRUE,
    TRUE ~ FALSE
  )) |>
  filter(centroid) |>
  select(-(secondary_territory:centroid)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# load poc data
pop <- refugees::population |>
  filter(year == max(year)) |>
  pivot_longer(refugees:hst, names_to = "pop_type", values_to = "pop_num") |>
  filter(!(pop_type %in% c("hst", "stateless", "ooc", "returned_refugees", "returned_idps"))) |>
  group_by(coa_iso) |>
  summarise(pop_tot = sum(pop_num, na.rm = TRUE)) |>
  ungroup() |>
  filter(coa_iso != "UNK")

# load vulnerability
vulnerability <- read_csv(here::here("input", "vulnerability.csv")) |>
  janitor::clean_names() |>
  select(iso3, last_col()) |>
  rename(vulnerability_2021 = x2021) |>
  filter(!is.na(vulnerability_2021))


# prepare map data --------------------------------------------------------

climate <- wrl_a_sf |>
  left_join(vulnerability, by = c("isoclr" = "iso3"))

pop_sf <- wrl_p_sf |>
  left_join(pop, by = c("iso3" = "coa_iso")) |>
  filter(!(is.na(pop_tot)))

# Camcorder ---------------------------------------------------------------

# camcorder::gg_record(
#   dir = "temp", # where to save the recording
#   device = agg_png, # device to use to save images
#   width = 1280 * 2 / 300, # width of saved image
#   height = 800 * 2 / 300, # height of saved image
#   units = "in", # units for width and height
#   dpi = 300 # dpi to use when saving image
# )



# Plot --------------------------------------------------------------------

title <- "Converging crises: forced displacement and climate vulnerability"
subtitle <- "The majority of the world’s refugees and displaced people live in highly climate-vulnerable areas"
disclaimer <- "<em>The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations.</em>"
source <- "<b>Note:</b> Vulnerability data for South Sudan is unavailable. <b>Sources:</b> UNHCR Refugee Data Finder, University of Notre Dame"


ggplot() +
  geom_sf(
    data = wrl_a_sf,
    fill = poly_col,
    color = "transparent"
  ) +
  geom_sf(
    data = filter(climate, !(is.na(vulnerability_2021))),
    aes(fill = vulnerability_2021),
    color = "transparent",
    # alpha = .85
  ) +
  geom_sf(
    data = wrl_l_sf,
    aes(linetype = bdystyle),
    color = "white",
    linewidth = .25
  ) +
  geom_sf(
    data = pop_sf,
    aes(size = pop_tot),
    shape = 21,
    fill = unhcr_pal(n = 5, "pal_navy")[3],
    color = unhcr_pal(n = 5, "pal_navy")[5],
    alpha = 0.6,
    linewidth = 3
  ) +
  scale_linetype_identity() +
  scale_fill_unhcr_c(
    direction = 1, palette = 4, na.value = poly_col,
    limits = c(.2, .8), breaks = c(.2, .5, .8)
  ) +
  scale_size_area(
    max_size = 10,
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    ),
    breaks = c(5e5, 2e6, 8e6)
  ) +
  coord_sf(crs = robin) +
  labs(
    title = title,
    subtitle = subtitle,
    fill = "Vulnerability index",
    size = "Displaced population",
    caption = paste0(disclaimer, "<br>", source)
  ) +
  theme_unhcr(void = TRUE, font_size = 13.5, legend_title = TRUE) +
  guides(size = guide_legend(
    theme = theme(
      legend.text.position = "bottom",
      legend.title = element_text(margin = margin(b = 0))
    )),
    fill = guide_colorbar(
        barheight = unit(3, units = "mm"))
  ) +
  theme(
    legend.position = "inside",
    legend.justification.inside = c(0, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(
      hjust = 0,
      size = 10.5
    ),
    legend.text = element_text(
      size = 9.5
    ),
    legend.spacing.y = unit(.15, "cm")
  )



# save ----------------------------------------------------------

# stop recording
# camcorder::gg_stop_recording()


# save plot
ggsave("climate_vulnerability_vs_displaced_pop.png",
       device = agg_png, dpi = 300,
       width = 1280 * 2 / 300, # width of saved image
       height = 800 * 2 / 300, # height of saved image
       units = "in", # units for width and height
       )
