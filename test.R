get_hosp_reg_tbl <- function(username,
                             date_column = "collection_date",
                             state_abbr  = 'state',
                             lag         = 2,
                             start_date  = "2020-01-01"){

    pop <- readr::read_rds(here::here("data/pop.rds")) |>



        pop %$%
        dplyr::group_by(fema_region) %$%
          dplyr::summarise(pop = sum(pop)) %$%
        View()



    read_csv(paste0("C:/Users/",
                    "ovt1",
                    "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/HOSPITAL DATA/hospitals_region_daily_incident_management.csv"),
             guess_max = 3000,
             show_col_types = FALSE)  |>
        clean_raw_data(date_column = "collection_date",
                       lag         = 2,
                       start_date  = "2020-01-01") |>
        select(fema_region,
               date,
               adm_all_covid_confirmed,
               avg_adm_all_covid_confirmed
        ) |>
        group_by(fema_region) |>
        mutate(hosp_adm    = adm_all_covid_confirmed,
               hosp_cum    = cumsum(hosp_adm),
               hosp_avg    = avg_adm_all_covid_confirmed |> round(),
               hosp_new    = adm_all_covid_confirmed,
               pop         = sum(pop)) |>
        group_by(date,fema_region) |>
        summarise(
            hosp_cum = hosp_cum %>% format(justify  = "right",
                                           big.mark = ","),
            hosp_new = hosp_new,
            hosp_avg = hosp_avg
        ) |>
        ungroup() |>
        group_by(fema_region) |>
        mutate(
            hosp_change = get_per_change(hosp_avg, date),
            hosp_7cum   = get_juris_7day_cum_per_100k(hosp_new, pop = pop)
        ) |>
        ungroup() |>
        rename(hosp_date = date)
