case_death_jur_tbl

timeframe <- "Daily"

region_colors <- read_rds(here::here("data/region_colors.rds"))


date_col <-  case_death_jur_tbl |>
    select(contains("date"),contains("cases"),pop,fema_region) |>
    select(1) |>
    colnames()

new_col <-  case_death_jur_tbl |>
    select(contains("new")) |>
    select(1) |>
    colnames()


data <-  case_death_jur_tbl |>
    rename(date  := {{date_col}},
           new   := {{new_col}}) |>
    mutate(date       = date  |> lubridate::ymd(),
           day        = format(date, "%b %d"),
           week       = lubridate::week(date),
           month      = lubridate::month(date,
                                         label = TRUE,
                                         abbr = TRUE),
           year       = lubridate::year(date),
           day_year   = glue::glue('{day}, {year}'),
           week_year  = glue::glue('{week}-{year}'),
           month_year = glue::glue('{month}-{year}'))


period <- if_else(timeframe == "Daily",  "day_year",
                  if_else(timeframe == "Weekly", "week_year",
                          if_else(timeframe == "Monthly", "month_year", "date")))

avg_range <- if_else(timeframe == "Daily",  7,
                     if_else(timeframe == "Weekly", 14,
                             if_else(timeframe == "Monthly",30,0)))
mutliplier <- if_else(timeframe == "Daily",  1,
                      if_else(timeframe == "Weekly", 7,
                              if_else(timeframe == "Monthly",30,0)))
fema_order <- c("Region 1","Region 2","Region 3","Region 4","Region 5","Region 6","Region 7","Region 8","Region 9","Region 10")

prepped_data <-  data |>
    rename(period := {{period}}) |>
    select(juris, date, new, period,pop,fema_region) |> arrange(date) |>
    mutate( avg_hold  = get_mean(new, days = avg_range)) |>
    mutate( avg_holder  = avg_hold * mutliplier) |>
    group_by(juris, period) |>
    summarise(date         = max(date),
              new          = sum(new),
              new_per_100k = new/pop *100000,
              avg          = avg_holder [which.max(date)],
              group        = 'group',
              fema_region  = fema_region) |> arrange(date) |>
    ungroup() |>
    mutate(period = fct_reorder(period,
                                date),
           fema_region = factor(fema_region, levels = c("Region 1","Region 2","Region 3","Region 4","Region 5","Region 6","Region 7","Region 8","Region 9","Region 10") ))

ylimit <- max(prepped_data$new_per_100k) * 1.1
ylimmit_14_day <- max(prepped_data$avg) * 1.1

prepped_data |>
    ggplot(aes(y = new_per_100k,
               x = period,
               fill= fema_region)) +
    geom_col() +
    scale_fill_manual(values = region_colors)  +
    # {if(timeframe != "Monthly")geom_line(aes(y = avg,
    #                                          group = group),
    #                                      size=1)} +

    scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    # theme(axis.text.x = element_text(angle = 40,
    #                                  vjust=0.5)) +
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
    geofacet::facet_geo(~ juris,  grid = "us_state_grid2", scales = "fixed") +
   # theme_ben() |>
    theme(strip.background =element_blank(),
          axis.line.x =  element_blank(),
          strip.text = element_text(size = rel(0.5),
                                    face = "bold",
                                    color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = rel(0.8), face = "bold"),
          legend.key = element_rect(fill = "white", colour = "white"),
         # legend.key.size = unit(0.5, "lines"),
          legend.background = element_rect(fill = "white", colour = "white"),
          legend.position="top"
          )+ guides(fill = guide_legend(nrow = 2))


prepped_data |>
    ggplot(aes(y = new_per_100k,
               x = period,
               fill= fema_region)) +
    geom_col() +
    scale_fill_manual(values = region_colors)  +
    # {if(timeframe != "Monthly")geom_line(aes(y = avg,
    #                                          group = group),
    #                                      size=1)} +

    scale_y_continuous(labels = scales::label_number(big.mark = ","),
                       guide = guide_axis(check.overlap = TRUE)) +
    # theme(axis.text.x = element_text(angle = 40,
    #                                  vjust=0.5)) +
    geofacet::facet_geo(~ juris,  grid = "us_state_grid2", scales = "fixed") +
    theme_bw() +
    theme(strip.background = element_blank(),
          axis.line   = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),

          strip.text = element_text(size = rel(0.5),
                                    face = "bold",
                                    color = "black"),
          legend.title = element_blank(),
          legend.text = element_text(size = rel(0.64), face = "bold"),
          legend.key = element_rect(fill = "white", colour = "white"),
          # legend.key.size = unit(0.5, "lines"),
          legend.background = element_rect(fill = "white", colour = "white"),
          legend.position="top")+
    guides(fill = guide_legend(nrow = 2))
