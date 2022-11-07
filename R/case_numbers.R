
#' Title
#'
#' @param dataset
#' @param indicator
#'
#' @return
#' @export
#'
#' @examples
numbers_exsum<-function(dataset,timeframe,indicator){
    date_col<-dataset |>
        select(contains("date")) |>
        select(1) |>
        colnames()
    cum_col<-dataset |>
        select(contains("cum")) |>
        select(1) |>
        colnames()
    new_col<-dataset |>
        select(contains("new")) |>
        select(1) |>
        colnames()
    avg_col<-dataset |>
        select(contains("avg")) |>
        select(1) |>
        colnames()
    prior_avg_col<-dataset |>
        select(contains("prior_avg")) |>
        select(1) |>
        colnames()
    change_col<-dataset |>
        select(contains("change")) |>
        select(1) |>
        colnames()
    dataset <-  dataset |>
        rename(date  := {{date_col}},
               cum   := {{cum_col}},
               new   := {{new_col}},
               avg   := {{avg_col}},
               prior_avg   := {{prior_avg_col}},
               change   := {{change_col}}
        )
    #get dates
    date_raw<-dataset%>%pull(date)
    date<-format(date_raw,format="%b %d, %Y")
    #get the case and death data current week start date
    date_6<- format(date_raw-6, format="%b %d, %Y")
    date_7<- format(date_raw-7, format="%b %d, %Y")
    date_13<- format(date_raw-13, format="%b %d, %Y")
    date_20<-format(date_raw-20, format="%b %d, %Y")
    #get the case and death data current 14 days end date
    date_21<-format(date_raw-21, format="%b %d, %Y")
    #get the case and death data current 14 days start date
    date_41<-format(date_raw-41, format="%b %d, %Y")
    #create case numbers dataframe
    number_list<-data.frame(
        totalnumber=dataset$cum,
        totalname=glue("Total {indicator} Reported"),
        daterange=if_else(indicator=="Daily",glue("Aug 01, 2020 - {date}"),glue("Jan 23, 2020 - {date}")),
        newnumber=dataset$new,
        newname=glue("New {indicator}"),
        daterange2=if_else(timeframe=="Daily",glue("{date}"),glue("{date_6} - {date}")),
        averagenumber=dataset$avg,
        averagename=if_else(timeframe=="Daily","Current 7-Day Average","Current 21-Day Average"),
        daterange3=if_else(timeframe=="Daily",glue("{date_6} - {date}"),glue("{date_20} - {date}")),
        prioraveragenumber=dataset$prior_avg,
        prioraveragename=if_else(timeframe=="Daily","Prior 7-Day Average","Prior 21-Day Average"),
        daterange4=if_else(timeframe=="Daily",glue("{date_13} - {date_7}"),glue("{date_41} - {date_21}")),
        change=dataset$change,
        changename=if_else(timeframe=="Daily","Change in 7-Day Average","Change in 21-Day Average"))%>%
        pivot_longer( cols = everything(),
                      names_to = "names",
                      values_to = "val",
                      values_transform = list(val = as.character))%>%select(val)%>%flextable()

    return(number_list)
}


#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_numbers_wh<-function(dataset,indicator){
    date_col<-dataset |>
        select(contains("date")) |>
        select(1) |>
        colnames()
    cum_col<-dataset |>select(-contains("7cum"))|>select(-contains("cum7"))|>
        select(contains("cum")) |>
        select(1) |>
        colnames()
    new_col<-dataset |>
        select(contains("new")) |>
        select(1) |>
        colnames()
    avg_col<-dataset |>
        select(contains("avg")) |>
        select(1) |>
        colnames()
    prior_avg_col<-dataset |>
        select(contains("prior_avg")) |>
        select(1) |>
        colnames()
    change_col<-dataset |>
        select(contains("change")) |>
        select(1) |>
        colnames()
    dataset <-  dataset |>
        rename(date  := {{date_col}},
               cum   := {{cum_col}},
               new   := {{new_col}},
               avg   := {{avg_col}},
               prior_avg   := {{prior_avg_col}},
               change   := {{change_col}}
        )
    end_date_raw<-dataset%>%pull(date)
    end_date<-format(end_date_raw,format="%m/%d/%y")
    end_date_6<- format(end_date_raw-6, format="%m/%d/%y")
    end_date_7<- format(end_date_raw-7, format="%m/%d/%y")
    end_date_13<- format(end_date_raw-13, format="%m/%d/%y")
    end_date_20<- format(end_date_raw-20, format="%m/%d/%y")
    end_date_21<- format(end_date_raw-21, format="%m/%d/%y")
    end_date_41<- format(end_date_raw-41, format="%m/%d/%y")
    numbers_list<-data.frame(
        totalname=glue("TOTAL {indicator} Reported Since 1/22/20"),
        totalnumber=dataset$cum,
        newname=glue("NEW {indicator} Reported to CDC on {end_date}"),
        newnumber=dataset$new,
        changename=glue("Change in 7-Day {indicator} Average"),
        change=dataset$change,
        averagename=glue("Current 7-Day {indicator} Average ({end_date_6} - {end_date})"),
        averagenumber=dataset$avg,
        prioraveragename=glue("Prior 7-Day {indicator} Average ({end_date_13} - {end_date_7})"),
        prioraveragenumber=dataset$prior_avg
    )%>%
        pivot_longer( cols = everything(),
                      names_to = "names",
                      values_to = "val",
                      values_transform = list(val = as.character))%>%select(val)

    return(numbers_list)
}
