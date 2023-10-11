# Burundi returns | Alluvial plot
# Data source: UNHCR Burundi

# setup -----------------------------------------------------------
# load packages
library(tidyverse)
library(ridl)
library(scales)
library(ragg)
library(unhcrthemes)
library(ggsankey)

# set ridl token
ridl::ridl_config_get()
# set local folder
local_folder <- "2023-09-bdi-return"

# load data ---------------------------------------------------------------
ridl_url <- "dd657014-aa18-4b44-91d1-1ac7005db802"

ret_raw <- ridl::rr_show(ridl_url) |>
    ridl::rr_read() |>
    janitor::clean_names()

# glimpse(ret_raw)

# wrangle data ------------------------------------------------------------
# calculate total returns by country of asylum
returnees <- ret_raw |>
    select(convoy_date, country_of_asylum) |>
    mutate(
        convoy_date = lubridate::ymd(convoy_date),
        country_of_asylum = str_to_title(country_of_asylum),
        top_coa = fct_lump_n(country_of_asylum, n = 5)
    ) |>
    group_by(top_coa) |>
    summarise(num_ret = n()) |>
    filter(top_coa != "Other") |>
    mutate(
        top_coa = case_when(
            top_coa == "Tanzania" ~ "United Rep. of Tanzania",
            top_coa == "Rdc" ~ "DR of the Congo",
            top_coa == "Ouganda" ~ "Uganda",
            TRUE ~ top_coa
        )
    )

# create refugees tribble as of 2023-08-31
refugees <- tribble(
    ~country, ~num_ref,
    "United Rep. of Tanzania", 159409,
    "DR of the Congo", 44971,
    "Uganda", 41132,
    "Rwanda", 48077,
    "Kenya", 30823,
)

# join refugees and returnees
ret_vs_ref <- returnees |>
    left_join(refugees, by = c("top_coa" = "country")) |>
    mutate(
        ret_perc = num_ret / (num_ret + num_ref),
        rest_perc = 1 - ret_perc,
        top_coa = str_wrap(top_coa, width = 20),
        top_coa = fct_reorder(top_coa, ret_perc)
    ) |>
    select(top_coa, ret_perc, rest_perc) |>
    pivot_longer(cols = -top_coa, names_to = "type", values_to = "perc")

# start recording ---------------------------------------------------------
camcorder::gg_record(
    dir = here::here("temp-plot"),
    scale = 1,
    device = agg_png,
    width = 7.5,
    height = 7.5 / 1.4,
    units = "in"
)

# plot --------------------------------------------------------------------
# title
title <- "Burundian Refugees Returning Home"

# subtitle
subtitle <-
    glue::glue(
        "Percentage of Burundian refugees that have returned by country of asylum",
        "<br>",
        "End of August 2023"
    )

# caption
caption <- "Source: UNHCR Burundi - © UNHCR, The UN Refugee Agency"

ggplot(
    ret_vs_ref,
    aes(x = perc, y = top_coa, fill = type)
) +
    geom_col(width = .7) +
    geom_text(
        data = filter(ret_vs_ref, type == "ret_perc"),
        aes(label = percent(perc)),
        # position = position_stack(vjust = .8),
        size = 10 / .pt,
        fontface = "bold",
        hjust = 1,
        nudge_x = -.01,
        color = "white"
    ) +
    scale_fill_unhcr_d(
        order = c(2, 1)
    ) +
    scale_x_continuous(
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        title = title,
        subtitle = subtitle,
        caption = caption
    ) +
    theme_unhcr(
        axis_title = FALSE, grid = FALSE, axis_text = "y",
        legend = FALSE
    )

# stop recording ----------------------------------------------------------
camcorder::gg_stop_recording()

# save plot ---------------------------------------------------------------

ggsave(
    here::here(local_folder, "bdi_returns_2023.png"),
    device = agg_png,
    width = 7.5,
    height = 7.5 / 1.4,
    units = "in",
    dpi = 300
)
