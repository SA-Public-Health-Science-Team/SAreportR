
#' Title Vertical list of date, cumulative, new, average, prior average, and change of an indicator in a dataset
#'
#' @param dataset
#' @param indicator could be "Cases", "Deaths", "Admissions", or others, lab dataset has special list of numbers with function of lab_numbers_exsum in lab_numbers
#' @param timeframe could be "Daily" or "Weekly"
#'
#' @return a flextable of vertical list of cumulative, new, average, prior average, and change numbers
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


#' Title Vertical list of cumulative, new, average, prior average, and change of cases or deaths data in the format of White House slides
#'
#' @param dataset Case or death data. Hospital numbers in White House Slides have separate functions called hospital_numbers_wh. No lab data is needed for the White House Slides
#' @param indicator could be "Cases" or "Deaths"
#'
#' @return vertical list of cumulative, new, average, prior average, and change numbers
#' @export
#'
#' @examples
weekly_numbers_wh<-function(dataset,indicator){
    date_col<-dataset |>
        select(contains("date")) |>
        select(1) |>
        colnames()
    cum_col<-dataset |>select(-contains("7cum"))|>
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
#' Title Vertical list of cumulative, new, average, prior average, and change of hospital data in the format of White House Slides
#'
#' @param timeframe  could be "Daily" or "Weekly"
#' @param dataset1 the whole period hospital data to get the maximal value of new admissions
#' @param dataset2 the last date of hospital data to get the most recent cumulative, average, and new numbers.
#'
#' @return a flextable of vertical list of cumulative, new, average, prior average, and change of hospital data
#' @export
#'
#' @examples
hospital_numbers_wh<-function(dataset1,dataset2,timeframe){
    date_col<-dataset1 |>
        select(contains("date")) |>
        select(1) |>
        colnames()

    new_col<-dataset1 |>
        select(contains("new")) |>
        select(1) |>
        colnames()
    dataset1 <-  dataset1 |>
        rename(date  := {{date_col}},
               new   := {{new_col}}        )
    date_col2<-dataset2 |>
        select(contains("date")) |>
        select(1) |>
        colnames()

    new_col2<-dataset2 |>
        select(contains("new")) |>
        select(1) |>
        colnames()
    avg_col<-dataset2 |>
        select(contains("avg")) |>
        select(1) |>
        colnames()
    prior_avg_col<-dataset2 |>
        select(contains("prior_avg")) |>
        select(1) |>
        colnames()
    change_col<-dataset2 |>
        select(contains("change")) |>
        select(1) |>
        colnames()
    dataset2 <-  dataset2 |>
        rename(date  := {{date_col2}},
               new   := {{new_col2}},
               avg   := {{avg_col}},
               prior_avg   := {{prior_avg_col}},
               change   := {{change_col}}
        )
    hospend_date_raw<-dataset2%>%pull(date)
    hospend_date<-format(hospend_date_raw,format="%m/%d/%y")
    hospend_date_6<- format(hospend_date_raw-6, format="%m/%d/%y")
    hospend_date_7<- format(hospend_date_raw-7, format="%m/%d/%y")
    hospend_date_13<- format(hospend_date_raw-13, format="%m/%d/%y")
    hospend_date_20<- format(hospend_date_raw-20, format="%m/%d/%y")
    hospend_date_21<- format(hospend_date_raw-21, format="%m/%d/%y")
    hospend_date_41<- format(hospend_date_raw-41, format="%m/%d/%y")
    #get the maximal admissions
    hosp_newpeak<-max(dataset1$new,na.rm=TRUE)
    #get the date of the maximal admissions
    hosp_newpeakdate<-dataset1%>%filter(new==hosp_newpeak)%>%head(1)%>%pull(date)
    #format the maximal admissions
    hosp_newpeakformat<-hosp_newpeak%>%format(justify="right",big.mark=",")
    #format the date of the maximal admissions
    hosp_newpeakdate<-format(hosp_newpeakdate,"%m/%d/%y")
    hosp_numbers_list<-data.frame(
        patientsname=glue("Patients Currently Hospitalized with COVID on {hospend_date}"),
        patientnumber=dataset2$total_patients,
        newname=glue("New Admissions on {hospend_date}"),
        newnumber=dataset2$new,
        peakname=glue("Peak in New Admissions({hosp_newpeakdate})"),
        admissionpeak=hosp_newpeakformat,
        changename=if_else(timeframe=="Daily","Change in 7-Day Average of New Admissioins","Change in 21-Day Average of New Admissioins"),
        change=dataset2$change,
        averagename=if_else(timeframe=="Daily",glue("Current 7-Day Average of New Admissions ({hospend_date_6} - {hospend_date})"),glue("Current 21-Day Average of New Admissions ({hospend_date_20} - {hospend_date})")),
        averagenumber=dataset2$avg,
        prioraveragename=if_else(timeframe=="Daily",glue("Prior 7-Day Average of New Admissions ({hospend_date_13} - {hospend_date_7})"),glue("Prior 21-Day Average of New Admissions ({hospend_date_41} - {hospend_date_21})")),
        prioraveragenumber=dataset2$prior_avg
    )%>%
        pivot_longer( cols = everything(),
                      names_to = "names",
                      values_to = "val",
                      values_transform = list(val = as.character))%>%select(val)%>%flextable()
    return(hosp_numbers_list)
}

#' Title Vertical list of cumulative, average, prior average, and change of lab data
#'
#' @param dataset the last date of lab data to get the most recent cumulative, average, and change numbers.
#' @param timeframe could be "Daily" or "Weekly"
#'
#' @return a flextable of vertical list of cumulative, average, prior average, and change of lab data
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
