# Burundi returns | Graph type
# Data source: EHAGL DIMA

local_folder <- "2023-09-bdi-return"

# load packages -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)
library(ragg)
library(unhcrthemes)
library(ggsankey)

# load data ---------------------------------------------------------------
ret_raw <- read_excel(here::here(
    local_folder,
    "Data_Masterlist_VolRep_Consolider 2021_Monthly_31_Aout_2023.xlsx"
)) |>
    janitor::clean_names()

# glimpse(ret_raw)

# wrangle data ------------------------------------------------------------
# calculate returnees by month
ret_num <- ret_raw |>
    select(convoy_date, country_of_asylum) |>
    mutate(
        convoy_date = lubridate::ymd(convoy_date),
        country_of_asylum = str_to_title(country_of_asylum),
        top_coa = fct_lump_n(country_of_asylum, n = 5, other_level = "Others"),
        month_date = lubridate::floor_date(convoy_date, unit = "month")
    ) |>
    group_by(top_coa, month_date) |>
    summarise(num_ret = n()) |>
    ungroup() |>
    pivot_wider(
        names_from = top_coa,
        values_from = num_ret,
        values_fill = 0
    ) |>
    arrange(month_date)

# View(ret_num)

# fix missing months
# create a dataframe with all months
date_range <- seq(min(ret_num$month_date),
    max(ret_num$month_date),
    by = "1 month"
)
data_range <- data.frame(month_date = date_range)

# merge the two dataframes
ret_num <- data_range |>
    left_join(ret_num, by = "month_date") |>
    replace_na(list(
        Kenya = 0,
        Ouganda = 0,
        Rdc = 0,
        Rwanda = 0,
        Tanzania = 0,
        Others = 0
    ))

# View(ret_num)

# calculate cumulative returns
ret_cumul <-
    ret_num |>
    rename(Uganda = Ouganda, `DR of the Congo` = Rdc, `United Rep. of Tanzania` = Tanzania) |>
    pivot_longer(cols = -month_date, names_to = "top_coa", values_to = "num_ret") |>
    arrange(top_coa, month_date) |>
    group_by(top_coa) |>
    mutate(cum_ret = cumsum(num_ret)) |>
    ungroup()

# View(ret_cumul)

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
subtitle <- "By Country of Asylum - Since 2017"

# caption
other_country <-
    ret_raw |>
    distinct(country_of_asylum) |>
    mutate(other_country = str_to_title(country_of_asylum)) |>
    filter(!(other_country %in% c("Kenya", "Ouganda", "Rdc", "Rwanda", "Tanzania"))) |>
    arrange(other_country) |>
    mutate(other_country = case_when(
        other_country == "Cameroun" ~ "Cameroon",
        other_country == "Congo Brazaville" ~ "Rep. of the Congo",
        other_country == "Guinee Bissau" ~ "Guinea Bissau",
        other_country == "Tchad" ~ "Chad",
        TRUE ~ other_country
    )) |>
    filter(other_country != "Zambie") |>
    pull(other_country)

other_country <- paste0(
    other_country,
    collapse = ", "
)

other_tot <- ret_cumul |>
    filter(top_coa == "Others") |>
    summarise(
        ret_sum = max(cum_ret)
    ) |>
    pull(ret_sum)

caption <- paste0(
    other_tot, " returnees came back from the following countries: ",
    other_country, ".<br>",
    "Source: UNHCR Burundi - © UNHCR, The UN Refugee Agency"
)

# annotation
annotation <-
    ret_cumul |>
    filter(top_coa != "Others") |>
    group_by(top_coa) |>
    summarise(
        x = max(month_date) + 20,
        ret_sum = max(cum_ret)
    ) |>
    ungroup() |>
    arrange(ret_sum) |>
    mutate(
        y = case_when(
            top_coa == "United Rep. of Tanzania" | top_coa == "Rwanda" ~ cumsum(ret_sum) - ret_sum / 3,
            TRUE ~ cumsum(ret_sum)
        ),
        label = case_when(
            top_coa == "Uganda" | top_coa == "Kenya" ~ paste0(
                top_coa, " - ",
                label_number(scale_cut = cut_short_scale())(ret_sum)
            ),
            top_coa == "United Rep. of Tanzania" ~ paste0(
                "United Rep. of", "\n", "Tanzania", "\n",
                label_number(scale_cut = cut_short_scale())(ret_sum)
            ),
            TRUE ~ paste0(
                top_coa, "\n",
                label_number(scale_cut = cut_short_scale())(ret_sum)
            )
        )
    )

ggplot(ret_cumul) +
    geom_sankey_bump(
        aes(
            x = month_date,
            node = top_coa,
            fill = top_coa,
            value = cum_ret,
            color = after_scale(colorspace::lighten(fill, 0.4))
        ),
        type = "alluvial",
        alpha = .8,
        space = 2e3,
        size = .2,
        smooth = 4
    ) +
    geom_text(
        data = annotation,
        aes(
            x = x,
            y = y,
            label = label,
            color = top_coa
        ),
        size = 9 / .pt,
        hjust = 0,
        lineheight = 1,
        fontface = "bold",
    ) +
    annotate("text",
        x = as.Date("2023-08-21"), y = 219e3,
        label = "Total returns\nsince 2017\n219.1K", hjust = 0,
        vjust = 0, lineheight = 1, fontface = "bold", size = 9 / .pt
    ) +
    scale_fill_unhcr_d(
        order = c(3, 5, 6, 2, 4, 1)
    ) +
    scale_color_unhcr_d(nmax = 6, order = c(3, 5, 2, 4, 1)) +
    scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y"
    ) +
    scale_y_continuous(
        expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(clip = "off", expand = FALSE) +
    labs(
        title = title,
        subtitle = subtitle,
        caption = caption
    ) +
    theme_unhcr(
        grid = FALSE, axis_title = FALSE,
        axis_ticks = "x", axis_text = "x",
        legend = FALSE, plot_margin = margin(12, 80, 12, 12),
        plot_title_margin = 4
    )

# stop recording ----------------------------------------------------------
camcorder::gg_stop_recording()

# save plot ---------------------------------------------------------------

ggsave(
    here::here(local_folder, "bdi_returns_2017_2023.png"),
    device = agg_png,
    width = 7.5,
    height = 7.5 / 1.4,
    units = "in",
    dpi = 300
)
