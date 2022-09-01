
plot_epicurve <- function(data_tbl, timeframe, indicator, color){


    date_col <-  case_death_juris_tbl |>
        select(contains("date")) |>
        select(1) |>
        colnames()

    new_col <-  case_death_juris_tbl |>
        select(contains("new")) |>
        select(1) |>
        colnames()


    data <-  case_death_juris_tbl |>
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
                 if_else(timeframe == "Monthly",0,0)))
    data |>
        rename(period := {{period}}) |>
        select(date, new, period) |> arrange(date) |>
        mutate( avg_holder  = get_mean(new, days = avg_range)) |>
        group_by(period) |>
        summarise(date  = max(date),
                  new   = sum(new),
                  avg   = avg_holder [which.max(date)] ,
                  group = 'group') |> arrange(date) |>
    ungroup() |>
    mutate(period = fct_reorder(period,
                                    date)) |>
        ggplot(aes(y = new,
               x = period)) +
        geom_col(fill="#f27f27")  +
        geom_line(aes(y = avg, group = group), size=1) +

        scale_y_continuous(labels = scales::label_number(big.mark = ","),
                           sec.axis =  sec_axis( trans=~./7, name="14-Day Average")) +
    theme( axis.text.x = element_text(angle = 40,vjust=0.5)) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
}

timeframe <- "Weekly"

data$new |> get_mean()



chart <- data |>
    rename(period := {{period}}) |>
    select(date, new, period) |> arrange(date) |>
    mutate( avg_holder  = get_mean(new, days = 14),
            avg_holder_7  = get_mean(new, days = 7)) |>
    group_by(period) |>
    summarise(date  = max(date),
              new   = sum(new),
              avg   = avg_holder [which.max(date)] ,
              avg_7   = avg_holder_7 [which.max(date)] ,
              group = 'group') |> arrange(date) |>
    ungroup() |>
    mutate(period = fct_reorder(period,
                                date)) |>
    ggplot(aes(y = new,
               x = period)) +
    geom_col(fill="blue")  +
    geom_line(aes(y = avg*7, group = group), size=1, color = "green") +
    geom_line(aes(y = avg_7*7, group = group), size=1, color = "orange") +

    scale_y_continuous(labels = scales::label_number(big.mark = ","),
                       sec.axis =  sec_axis( trans=~./7, name="Second Axis")) +
    theme( axis.text.x = element_text(angle = 40,vjust=0.5)) +
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

chart |>
    ggplotly()


 chart2 <- data |>
    rename(period := {{period}}) |>
    select(date, new, period) |> arrange(date) |>
    mutate( avg_holder  = get_mean(new, days = 14),
            avg_holder_7  = get_mean(new, days = 7)) |>
    group_by(period) |>
    summarise(date  = max(date),
              new   = sum(new),
              avg   = avg_holder [which.max(date)] ,
              avg_7   = avg_holder_7 [which.max(date)] ,
              group = 'group') |> arrange(date) |>
    ungroup() |>
    mutate(period = fct_reorder(period,
                                date)) |>
    ggplot(aes(y = new,
               x = period)) +
    geom_col(fill="blue")  +
    geom_line(aes(y = avg, group = group), size=1, color = "green") +
    geom_line(aes(y = avg_7, group = group), size=1, color = "orange") +

    scale_y_continuous(labels = scales::label_number(big.mark = ","),
                       sec.axis =  sec_axis( trans=~./2, name="Second Axis")) +
    theme( axis.text.x = element_text(angle = 40,vjust=0.5)) +
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


chart2 |>
     ggplotly()
