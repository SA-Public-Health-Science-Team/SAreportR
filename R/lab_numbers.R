#' Load, clean, and analyze state hospital data
#'
#' @param dataset
#'
#' @export
#' @import dplyr
#' @return returns aggregate hospital table with metrics
#' @export
#'
#' @examples
#'
daily_lab_numbers_exsum<-function(dataset){
    #get lab dates
    lab_date_raw<-Sys.Date()-3
    lab_date<-format(lab_date_raw,format="%b %d, %Y")
    lab_date_4<-format(Sys.Date()-7,format="%b %d, %Y")
    lab_date_10<-format(Sys.Date()-13,format="%b %d, %Y")
    lab_date_11<-format(Sys.Date()-14,format="%b %d, %Y")
    lab_date_17<-format(Sys.Date()-20,format="%b %d, %Y")
    lab_date_6<-format(Sys.Date()-9,format="%b %d, %Y")
    lab_date_7<-format(Sys.Date()-10,format="%b %d, %Y")
    lab_date_13<-format(Sys.Date()-16,format="%b %d, %Y")
    #create lab numbers dataframe
    lab_numbers_list<-data.frame(
        totalnumber=dataset$testvol_cum,
        totalname="Total Test volume",
        avg=dataset$testvol_avg,
        avgname="Current 7-Day Avg. Daily Test Volume",
        daterange=glue("{lab_date_10} - {lab_date_4}"),
        prioravg=dataset$testvol_prior_avg7,
        prioravgname="Prior 7-Day Avg. Daily Test volume",
        daterange2=glue("{lab_date_17} - {lab_date_11}"),
        change=dataset$testvol_change,
        prioraveragename="Percent Change in 7-Day Avg.",
        posavg=dataset$testpos_avg,
        posavgname="Current 7-Day Avg. % Positivity",
        daterange3=glue("{lab_date_6} - {lab_date}"),
        posprioravg=dataset$testpos_prior_avg7,
        priorposavgname="Prior 7-Day Avg. % Positivity",
        daterange4=glue("{lab_date_13} - {lab_date_7}"),
        poschange=dataset$testpos_change,
        poschangename="Percent Change in 7-Day Avg",
        poschangedif=dataset$testpos_difference,
        poschangedifname="Percentage Point Different in 7-Day Averages")%>%
        pivot_longer(cols=everything(),
                     names_to="names",
                     values_to="val",
                     values_transform=list(val=as.character))%>%select(val)%>%flextable()
    return(lab_numbers_list)
}
#' Title
#'
#' @param dataset
#' @param timeframe
#'
#' @return
#' @export
#'
#' @examples
lab_numbers_exsum<-function(dataset,timeframe){
    date_col<-dataset |>
        select(contains("date")) |>
        select(1) |>
        colnames()
    cum_col<-dataset |>
        select(contains("cum")) |>
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
    average_col<-dataset |>
        select(contains("average")) |>
        select(1) |>
        colnames()
    prior_average_col<-dataset |>
        select(contains("prior_average")) |>
        select(1) |>
        colnames()
    pos_change_col<-dataset |>
        select(contains("pos_change")) |>
        select(1) |>
        colnames()
    difference_col<-dataset |>
        select(contains("difference")) |>
        select(1) |>
        colnames()
    dataset <-  dataset |>
        rename(date  := {{date_col}},
               cum   := {{cum_col}},
               avg   := {{avg_col}},
               prior_avg   := {{prior_avg_col}},
               change   := {{change_col}},
               average   := {{average_col}},
               prior_average   := {{prior_average_col}},
               pos_change  := {{pos_change_col}},
               difference  := {{difference_col}}

        )
    #get lab dates
    lab_date_raw<-Sys.Date()-3
    lab_date<-format(lab_date_raw,format="%b %d, %Y")
    lab_date_4<-format(Sys.Date()-7,format="%b %d, %Y")
    lab_date_10<-format(Sys.Date()-13,format="%b %d, %Y")
    lab_date_11<-format(Sys.Date()-14,format="%b %d, %Y")
    lab_date_17<-format(Sys.Date()-20,format="%b %d, %Y")
    lab_date_6<-format(Sys.Date()-9,format="%b %d, %Y")
    lab_date_7<-format(Sys.Date()-10,format="%b %d, %Y")
    lab_date_13<-format(Sys.Date()-16,format="%b %d, %Y")
    lab_date_24<-format(Sys.Date()-27,format="%b %d, %Y")
    lab_date_25<-format(Sys.Date()-28,format="%b %d, %Y")
    lab_date_45<-format(Sys.Date()-48,format="%b %d, %Y")
    lab_date_20<-format(Sys.Date()-23,format="%b %d, %Y")
    lab_date_21<-format(Sys.Date()-24,format="%b %d, %Y")
    lab_date_41<-format(Sys.Date()-44,format="%b %d, %Y")
    #create lab numbers dataframe
    lab_numbers_list<-data.frame(
        totalnumber=dataset$cum,
        totalname="Total Test volume",
        avg=dataset$avg,
        avgname=if_else(timeframe=="Daily","Current 7-Day Avg. Daily Test Volume","Current 21-Day Avg. Daily Test Volume"),
        daterange=if_else(timeframe=="Daily",glue("{lab_date_10} - {lab_date_4}"),glue("{lab_date_24} - {lab_date_4}")),
        prioravg=dataset$prior_avg,
        prioravgname=if_else(timeframe=="Daily","Prior 7-Day Avg. Daily Test volume","Prior 21-Day Avg. Daily Test volume"),
        daterange2=if_else(timeframe=="Daily",glue("{lab_date_17} - {lab_date_11}"),glue("{lab_date_45} - {lab_date_25}")),
        change=dataset$change,
        prioraveragename=if_else(timeframe=="Daily","Percent Change in 7-Day Avg.","Percent Change in 21-Day Avg."),
        posavg=dataset$average,
        posavgname="Current 7-Day Avg. % Positivity",
        daterange3=if_else(timeframe=="Daily",glue("{lab_date_6} - {lab_date}"),glue("{lab_date_20} - {lab_date}")),
        posprioravg=dataset$prior_average,
        priorposavgname=if_else(timeframe=="Daily","Prior 7-Day Avg. % Positivity","Prior 21-Day Avg. % Positivity"),
        daterange4=if_else(timeframe=="Daily",glue("{lab_date_13} - {lab_date_7}"),glue("{lab_date_41} - {lab_date_21}")),
        poschange=dataset$pos_change,
        poschangename=if_else(timeframe=="Daily","Percent Change in 7-Day Avg","Percent Change in 21-Day Avg"),
        poschangedif=dataset$difference,
        poschangedifname=if_else(timeframe=="Daily","Percentage Point Different in 7-Day Averages","Percentage Point Different in 21-Day Averages"))%>%
        pivot_longer(cols=everything(),
                     names_to="names",
                     values_to="val",
                     values_transform=list(val=as.character))%>%select(val)%>%flextable()
    return(lab_numbers_list)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_lab_numbers_exsum<-function(dataset){
    #get lab dates
    lab_date_raw<-Sys.Date()-3
    lab_date<-format(lab_date_raw,format="%b %d, %Y")
    lab_date_4<-format(Sys.Date()-7,format="%b %d, %Y")
    lab_date_10<-format(Sys.Date()-13,format="%b %d, %Y")
    lab_date_11<-format(Sys.Date()-14,format="%b %d, %Y")
    lab_date_24<-format(Sys.Date()-27,format="%b %d, %Y")
    lab_date_25<-format(Sys.Date()-28,format="%b %d, %Y")
    lab_date_45<-format(Sys.Date()-48,format="%b %d, %Y")
    lab_date_6<-format(Sys.Date()-9,format="%b %d, %Y")
    lab_date_7<-format(Sys.Date()-10,format="%b %d, %Y")
    lab_date_20<-format(Sys.Date()-23,format="%b %d, %Y")
    lab_date_21<-format(Sys.Date()-24,format="%b %d, %Y")
    lab_date_41<-format(Sys.Date()-44,format="%b %d, %Y")
    #create lab numbers dataframe
    lab_numbers_list<-data.frame(
        totalnumber=dataset$testvol_cum,
        totalname="Total Test volume",
        avg=dataset$testvol_avg21,
        avgname="Current 21-Day Avg. Daily Test Volume",
        daterange=glue("{lab_date_24} - {lab_date_4}"),
        prioravg=dataset$testvol_prior_avg21,
        prioravgname="Prior 21-Day Avg. Daily Test volume",
        daterange2=glue("{lab_date_45} - {lab_date_25}"),
        change=dataset$testvol_change21,
        prioraveragename="Percent Change in 21-Day Avg.",
        posavg=dataset$testpos_avg21,
        posavgname="Current 21-Day Avg. % Positivity",
        daterange3=glue("{lab_date_20} - {lab_date}"),
        posprioravg=dataset$testpos_prior_avg21,
        priorposavgname="Prior 21-Day Avg. % Positivity",
        daterange4=glue("{lab_date_41} - {lab_date_21}"),
        poschange=dataset$testpos_change21,
        poschangename="Percent Change in 21-Day Avg",
        poschangedif=dataset$testpos_difference21,
        poschangedifname="Percentage Point Different in 21-Day Averages")%>%
        pivot_longer(cols=everything(),
                     names_to="names",
                     values_to="val",
                     values_transform=list(val=as.character))%>%select(val)%>%flextable()

    return(lab_numbers_list)
}

